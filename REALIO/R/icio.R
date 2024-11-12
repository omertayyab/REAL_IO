
library(tidyverse)
library(here)
library(readr)
library(networkD3)


function(file1, file2, cntry_1,  ) {


  icio <- read_csv(file1)
  conc <- read_csv(file2, n_max = 44) %>% select(1:4)

  cntry_1 <- "PAK"


  imp <- icio %>%
    select(starts_with(c("V1",cntry_1))) %>%
    filter(str_detect(V1, cntry_1, negate=TRUE)) %>%
    slice(1:(n()-3))
  imp$V1 <- gsub("^.{0,4}","", imp$V1)
  imp <- imp %>%
    summarize(across(1:last_col(), sum), .by = V1)


  dom <- icio %>%
    select(starts_with(c("V1",cntry_1))) %>%
    filter(str_detect(V1, cntry_1))
  dom$V1 <- gsub("^.{0,4}","", dom$V1)


  exp <- icio %>%
    filter(str_detect(V1, cntry_1)) %>%
    select(!starts_with(c(cntry_1,"OUT"))) %>%
    mutate(exp = rowSums(across(2:last_col())), .keep = "unused")

  Xd <- mutate(dom,
               int = rowSums(across(2:46)),
               final = rowSums(across(47:52)),
               total = rowSums(across(2:52)),
               .keep="unused")
  Xm <- mutate(imp,
               int = rowSums(across(2:46)),
               final = rowSums(across(47:52)),
               total = rowSums(across(2:52)),
               .keep="unused")

  X_tot <- Xd$int + Xd$final + Xm$int + Xm$final + exp$exp
  X_tot <- X_tot[-length(X_tot)]


  A_mat <- select(dom, c(2:45)) %>%
    slice(-n())
  A_mat <- rbind(A_mat, (Xd$total[-45] + exp$exp[-45])) %>%
    lapply(., function(x)x/x[length(x)]) %>%
    as_tibble %>%
    slice(-n()) %>%
    as.matrix

  Am_mat <- select(imp, 2:45) %>%
    slice(-n())
  Am_mat <-rbind(Am_mat, (Xd$total[-45] + exp$exp[-45])) %>%
    lapply(., function(x)x/x[length(x)]) %>%
    as_tibble %>%
    slice(-n()) %>%
    as.matrix

  L_mat <- (diag(dim(A_mat)[1]) - A_mat ) %>% solve()

  emb_imp <- (Am_mat %*% L_mat)
  emb_imp <- zapsmall(emb_imp, digits = 4)


  #####setup for sankey######
  conc$Name <- paste(conc$Name, conc$BECv5)
  nodes <- c(conc$Name, conc$DLS) %>% unique

  conc$target <- match(conc$DLS, table = nodes)-1   #minus 1 because Sankey function works as 0-indexed
  conc$source <- match(conc$Name, table = nodes)-1

  #####################################


  final_1 <-
    tibble(
      DLS= conc$DLS[-45],
      emb_imp = colSums(emb_imp),
      dom = colSums(L_mat),
      imp_final = Xm$final[-45],
      dom_final = Xd$final[-45],
      exports = exp$exp[-45]
    ) %>%
    mutate(emb_imp_final = emb_imp * Xd$final[-45]) %>%
    mutate(emb_imp_exports = emb_imp * exp$exp[-45])

  result_1  <- final_1 %>%
    group_by(DLS) %>%
    summarise_all(sum) %>%
    mutate(ov_imp = imp_final + emb_imp_final + emb_imp_exports) %>%
    #select(-c("imports", "domestic")) %>%
    arrange(desc(ov_imp))



  #######################  Sankey  ######################


  link1 <-
    tibble(source = conc$source,
           target = conc$target,
           value = final_1$dom_final,
           group = "domestic"
    ) %>%
    group_by(source, target, group) %>%
    summarize(value= sum(value))

  link2 <-
    tibble(source = conc$source,
           target = conc$target,
           value = final_1$imp_final,
           group = "import"
    ) %>%
    group_by(source, target, group) %>%
    summarize(value= sum(value))

  link <-
    rbind(link1, link2) %>%
    as.data.frame

  nodesD <- data.frame(names = nodes)
  nodesD$group <- c(rep("production", 44), rep("DLS",11))

  my_color <- 'd3.scaleOrdinal()
              .domain(["domestic", "import", "production", "DLS"])
              .range(["Grey", "Coral", "#69b3a2", "steelblue"])'

  p1 <- sankeyNetwork(Links = link, Nodes = nodesD, Source = "source",
                      Target = "target", Value = "value", NodeID = "names",
                      colourScale=my_color,
                      NodeGroup = "group", fontSize = 14,
                      height = 1200, width = 1000)
  p1
  ###########################################################

  ######### Divide imports into GN and GS ############

  cntry_list <- read_csv(file2) %>% select(7:9)
  conc <- read_csv(file2, n_max = 44) %>% select(1:4)
  conc$Name <- paste(conc$Name, conc$BECv5)


  cntry_1 <- "PAK"
  n_ind <- 45

  cntry_GS <- filter(cntry_list, GS_GN== "GS")
  cntry_GN <- filter(cntry_list, GS_GN== "GN")


  #Imports

  imp_GS <- icio %>%
    select(starts_with(c("V1",cntry_1))) %>%
    filter(str_detect(V1, cntry_1, negate=TRUE))

  imp_GS$V1 <- substr(imp_GS$V1, 1,3)

  imp_GS <- imp_GS %>%
    subset(V1 %in% cntry_GS$Code)

  imp_GS$V1 <- rep(1:n_ind, length(imp_GS$V1)/n_ind) %>% as.factor()

  imp_GS <- imp_GS %>%
    summarize(across(1:last_col(), sum), .by = V1)

  imp_GS$f <- rowSums(imp_GS[47:52])


  imp_GN <- icio %>%
    select(starts_with(c("V1",cntry_1))) %>%
    filter(str_detect(V1, cntry_1, negate=TRUE))

  imp_GN$V1 <- substr(imp_GN$V1, 1,3)

  imp_GN <- imp_GN %>%
    subset(V1 %in% cntry_GN$Code)

  imp_GN$V1 <- rep(1:n_ind, length(imp_GN$V1)/n_ind) %>% as.factor()

  imp_GN <- imp_GN %>%
    summarize(across(1:last_col(), sum), .by = V1)

  imp_GN$f <- rowSums(imp_GN[47:52])
  # Exports

  exp_GS <- icio %>%
    filter(str_detect(V1, cntry_1)) %>%
    select(!starts_with(c(cntry_1,"OUT"))) %>%
    select(c(1,starts_with(cntry_GS$Code))) %>%
    mutate(exp = rowSums(across(2:last_col())), .keep = "unused")


  exp_GN <- icio %>%
    filter(str_detect(V1, cntry_1)) %>%
    select(!starts_with(c(cntry_1,"OUT"))) %>%
    select(c(1,starts_with(cntry_GN$Code))) %>%
    mutate(exp = rowSums(across(2:last_col())), .keep = "unused")


  # Separate Matrices for GS and GN

  Am_mat_GS <- select(imp_GS, 2:45) %>%
    slice(-n())
  Am_mat_GS <-rbind(Am_mat_GS, (Xd$total[-45] + exp$exp[-45])) %>%
    lapply(., function(x)x/x[length(x)]) %>%
    as_tibble %>%
    slice(-n()) %>%
    as.matrix

  Am_mat_GN <- select(imp_GN, 2:45) %>%
    slice(-n())
  Am_mat_GN <-rbind(Am_mat_GN, (Xd$total[-45] + exp$exp[-45])) %>%
    lapply(., function(x)x/x[length(x)]) %>%
    as_tibble %>%
    slice(-n()) %>%
    as.matrix

  emb_imp_GS <- (Am_mat_GS %*% L_mat)
  emb_imp_GS <- zapsmall(emb_imp_GS, digits = 5)

  emb_imp_GN <- (Am_mat_GN %*% L_mat)
  emb_imp_GN <- zapsmall(emb_imp_GN, digits = 5)

  final_2 <-
    tibble(
      DLS= conc$DLS[-45],
      emb_imp_GS = colSums(emb_imp_GS),
      emb_imp_GN = colSums(emb_imp_GN),
      dom = colSums(L_mat),
      imp_final = Xm$final[-45],
      imp_GS_final = imp_GS$f[-45],
      imp_GN_final = imp_GN$f[-45],
      dom_final = Xd$final[-45],
      exports = exp$exp[-45]
    ) %>%
    mutate(emb_imp_GS_final = emb_imp_GS * (dom_final+exports)) %>%
    mutate(emb_imp_GN_final = emb_imp_GN * (dom_final+exports))

  result_2  <- final_2 %>%
    group_by(DLS) %>%
    summarise_all(sum) %>%
    mutate(ov_imp = imp_final + emb_imp_GS_final + emb_imp_GN_final) %>%
    #select(-c("imports", "domestic")) %>%
    arrange(desc(ov_imp))


  ###### Breakdown of individual sectors#########

  emb_imp_sector <- map2(as_tibble(emb_imp),Xd$final[-45], ~.x * .y ) %>%
    as_tibble() %>%
    as.matrix() %>%
    t() %>%
    as_tibble %>%
    mutate(DLS = conc$DLS) %>%
    group_by(DLS) %>%
    summarize_all(sum)

  cname <- emb_imp_sector[[1]]

  emb_imp_sector <- emb_imp_sector %>%
    select(-1) %>%
    t() %>%
    as_tibble

  colnames(emb_imp_sector) <- cname

  emb_imp_sector$sector <- conc$Name




  ############Sankeys###########################



  nodes <- c(conc$Name, conc$DLS) %>% unique
  nodesD <- data.frame(names = nodes)
  nodesD$group <- c(rep("production", 44), rep("DLS",11))

  link_by_DLS <- emb_imp_sector %>%
    pivot_longer(cols = !sector,
                 names_to = "DLS",
                 values_to = "value"
    )

  link_by_DLS$source <- match(link_by_DLS$sector, table = nodesD$names)-1
  link_by_DLS$target <- match(link_by_DLS$DLS, table = nodesD$names)-1
  link_by_DLS$group <- "Embodied"


  link_by_DLS_2 <- tibble(
    sector = conc$Name,
    DLS = conc$DLS,
    value = final_1$imp_final,
    source = match(conc$Name, table = nodesD$names)-1,
    target = match(conc$DLS, table = nodesD$names)-1,
    group = "Final"
  )

  link_by_DLS <- rbind(link_by_DLS, link_by_DLS_2)

  my_color <- 'd3.scaleOrdinal()
              .domain(["Embodied", "Final", "production", "DLS"])
              .range(["grey", "LightCoral", "#69b3a2", "steelblue"])'


  p2 <- sankeyNetwork(Links = link_by_DLS, Nodes = nodesD, Source = "source",
                      Target = "target", Value = "value", NodeID = "names",
                      LinkGroup = "group", colourScale=my_color,
                      NodeGroup = "group", fontSize = 14,
                      height = 1400, width = 1400
  )
  p2



  link_by_Origin <- pivot_longer(result_2, cols = c("imp_GS_final","imp_GN_final",
                                                    "emb_imp_GS_final", "emb_imp_GN_final" ),
                                 names_to = "Origin", values_to = "value") %>%
    select(c(1,9,10)) %>%
    mutate(group = rep(c("Final", "Final", "Embodied", "Embodied"), 11)) %>%
    mutate(Origin = str_sub(Origin, -8, -7))

  nodes <- c(link_by_Origin$Origin, link_by_Origin$DLS) %>% unique

  link_by_Origin$source <- match(link_by_Origin$Origin, table = nodes)-1
  link_by_Origin$target <- match(link_by_Origin$DLS, table = nodes)-1



  my_color <- 'd3.scaleOrdinal()
              .domain(["Embodied", "Final", "Origin", "DLS"])
              .range(["grey", "LightCoral", "#69b3a2", "steelblue"])'

  nodesD3 <- data.frame(names=nodes, group = c(rep("Origin", 2), rep("DLS", 11)))

  p3 <- sankeyNetwork(Links = link_by_Origin, Nodes = nodesD3, Source = "source",
                      Target = "target", Value = "value", NodeID = "names",
                      colourScale=my_color, LinkGroup="group",
                      NodeGroup = "group", fontSize = 14)
  p3




  DLS_ch <- "Nutrition"
  cutoff <- 10

  sel_ind <-  which( final_2$DLS %in% DLS_ch)

  nodesN <- nodesD
  nodesN$names <- paste(nodesD$names, "N")

  link_sel_GS <- sapply(sel_ind, function(x) emb_imp_GS[,x] *
                          (final_2$dom_final[x] + final_2$exports[x] )) %>%
    rowSums() %>%
    as_tibble %>%
    mutate(sector = conc$Name, group = "Embodied") %>%
    rbind(.,tibble(sector = filter(conc, DLS == DLS_ch)$Name,
                   value = filter(final_2, DLS == DLS_ch)$imp_GS_final,
                   group = "Final")
    ) %>%
    filter(value > cutoff)

  link_sel_GN <- sapply(sel_ind, function(x) emb_imp_GN[,x] *
                          (final_2$dom_final[x] + final_2$exports[x] )) %>%
    rowSums() %>%
    as_tibble() %>%
    mutate(sector = paste(conc$Name, "N"), group = "Embodied") %>%
    rbind(.,tibble(sector = paste(filter(conc, DLS == DLS_ch)$Name, "N" ),
                   value = filter(final_2, DLS == DLS_ch)$imp_GN_final,
                   group = "Final")
    ) %>%
    filter(value > cutoff)

  nodes_sel <- rbind(nodesD, nodesN) %>%
    filter(names %in% c(link_sel_GS$sector, link_sel_GN$sector)| names == DLS_ch)

  link_sel_GS <- link_sel_GS %>%
    mutate(source = match(sector, nodes_sel$names) -1,
           target = match(DLS_ch, nodes_sel$names) -1)

  link_sel_GN <- link_sel_GN %>%
    mutate(target = match(sector, nodes_sel$names) -1,
           source = match(DLS_ch, nodes_sel$names) -1)


  link_sel <- rbind(link_sel_GS, link_sel_GN)



  p4 <- sankeyNetwork(Links = link_sel ,
                      Nodes = nodes_sel,
                      Source = "source",
                      Target = "target", Value = "value", NodeID = "names",
                      LinkGroup = "group", colourScale=my_color,
                      NodeGroup = "group", fontSize = 14,
                      height = 1000, width = 1400
  )
  p4
}
