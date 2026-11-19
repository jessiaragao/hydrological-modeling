# hidrosed.R.Tradução

# Uso um environment para simular variáveis globais mutáveis.
dados <- new.env()
dados$titulo <- ""
dados$dt <- 0.0; dados$NQ <- 0L
dados$R  <- numeric(10); dados$K  <- numeric(10); dados$LS <- numeric(10)
dados$CP <- numeric(10); dados$AQ <- numeric(10)
dados$ERE <- numeric(10); dados$ER <- numeric(10)
dados$ERT <- 0.0; dados$AB <- 0.0; dados$PS <- 0.0
dados$arqent <- ""; dados$arqsai <- ""
dados$arqsdr <- ""; dados$NSDR <- 0L; dados$NBRUNE <- 0L
dados$LM <- 0.0; dados$FR1 <- 0.0; dados$FR2 <- 0.0; dados$RB <- 0.0
dados$SM <- 0.0; dados$SDR <- 0.0; dados$TR <- 0.0; dados$ETA <- 0.0

# Handles de arquivo (equivalente a OPEN com unidades 11 e 12)
f11 <- NULL
f12 <- NULL

entrada <- function() {
  dados$arqent <- readline("Digite o nome do arquivo geral de entrada: ")

  f11 <<- file(dados$arqent, "r")
  linha <- strsplit(readLines(f11, n = 1), "\\s+")[[1]]
  dados$titulo <- if (length(linha) >= 1) linha[1] else ""
  dados$arqsai <- if (length(linha) >= 2) linha[2] else ""
  dados$dt     <- if (length(linha) >= 3) as.numeric(linha[3]) else 0
  dados$NQ     <- if (length(linha) >= 4) as.integer(linha[4]) else 0L
  dados$arqsdr <- if (length(linha) >= 5) linha[5] else ""

  f12 <<- file(dados$arqsdr, "r")
  linha <- strsplit(readLines(f12, n = 1), "\\s+")[[1]]
  dados$NBRUNE <- if (length(linha) >= 1) as.integer(linha[1]) else 0L
  dados$ETA    <- if (length(linha) >= 2) as.numeric(linha[2]) else 0

  linha <- strsplit(readLines(f12, n = 1), "\\s+")[[1]]
  dados$NSDR <- if (length(linha) >= 1) as.integer(linha[1]) else 0L

  NC <- 1L
  while (NC <= dados$NQ) {
    linha <- strsplit(readLines(f11, n = 1), "\\s+")[[1]]
    dados$R[NC]  <- if (length(linha) >= 1) as.numeric(linha[1]) else 0
    dados$K[NC]  <- if (length(linha) >= 2) as.numeric(linha[2]) else 0
    dados$LS[NC] <- if (length(linha) >= 3) as.numeric(linha[3]) else 0
    dados$CP[NC] <- if (length(linha) >= 4) as.numeric(linha[4]) else 0
    dados$AQ[NC] <- if (length(linha) >= 5) as.numeric(linha[5]) else 0
    NC <- NC + 1L
  }
}

calculo <- function() {
  NC <- 1L
  dados$ERT <- 0; dados$AB <- 0
  while (NC <= dados$NQ) {
    dados$ERE[NC] <- dados$R[NC] * dados$K[NC] * dados$LS[NC] * dados$CP[NC]
    dados$ER[NC]  <- dados$ERE[NC] * dados$AQ[NC]
    dados$ERT <- dados$ERT + dados$ER[NC]
    dados$AB  <- dados$AB  + dados$AQ[NC]
    NC <- NC + 1L
  }

  if (dados$NSDR == 1) maner()
  else if (dados$NSDR == 2) roehl()
  else if (dados$NSDR == 3) williams()

  dados$PS <- dados$ERT * dados$SDR / (dados$AB * dados$dt)

  if (dados$NBRUNE == 1) {
    dados$ETA <- 100 / (1 + exp(-21.881 * dados$TR))
  } else if (dados$NBRUNE == 2) {
    dados$ETA <- 100 / (1 + exp(-18.146 * dados$TR))
  } else if (dados$NBRUNE == 3) {
    dados$ETA <- 92.07 / (1 + exp(-14.867 * dados$TR))
  }

  cat("ETA = ", dados$ETA, "\n")
}

maner <- function() {
  linha <- strsplit(readLines(f12, n = 1), "\\s+")[[1]]
  dados$LM  <- if (length(linha) >= 1) as.numeric(linha[1]) else 0
  dados$FR1 <- if (length(linha) >= 2) as.numeric(linha[2]) else 0
  dados$FR2 <- if (length(linha) >= 3) as.numeric(linha[3]) else 0

  FR <- dados$FR1 - dados$FR2
  if (FR <= 0) {
    cat("Inconsistencia nos dados topograficos!\n")
    readline("Pressione ENTER para continuar...")
  } else {
    dados$SDR <- (10^(2.943 - 0.824 * log10(dados$LM / FR))) / 100
  }
}

roehl <- function() {
  ABmi <- dados$AB * 0.003861  # ha -> mi^2

  linha <- strsplit(readLines(f12, n = 1), "\\s+")[[1]]
  dados$LM  <- if (length(linha) >= 1) as.numeric(linha[1]) else 0
  dados$FR1 <- if (length(linha) >= 2) as.numeric(linha[2]) else 0
  dados$FR2 <- if (length(linha) >= 3) as.numeric(linha[3]) else 0
  dados$RB  <- if (length(linha) >= 4) as.numeric(linha[4]) else 0

  FR <- dados$FR1 - dados$FR2
  if (FR <= 0) {
    cat("Inconsistencia nos dados topograficos!\n")
    readline("Pressione ENTER para continuar...")
  } else {
    # ATENÇÃO: linha quebrada no Fortran original. Assumi sinal +.
    dados$SDR <- (10^(4.27
                      - 0.23 * log10(ABmi)
                      - 0.51 * log10(dados$LM / FR)
                      + 2.79 * log10(dados$RB))) / 100
  }
}

williams <- function() {
  linha <- strsplit(readLines(f12, n = 1), "\\s+")[[1]]
  dados$SM <- if (length(linha) >= 1) as.numeric(linha[1]) else 0
  dados$SDR <- (62.7 * (dados$SM^0.403)) / 100
}

saida <- function() {
  cat("            Universidade Federal do Ceara\n")
  cat("             Centro de Ciencias Agrarias\n")
  cat("          Departamento de Engenharia Agricola\n")
  cat("                 Curso de Agronomia\n")
  cat("  Programadores: Jessica Aragao e Jose Carlos de Araujo\n")
  cat("                          Programa Hidrosed\n")
  cat("                           Fortaleza, 2020\n")
  cat("   O programa Hidrosed vem com finalidade de calcular o SDR utilizando\n")
  cat("                o metodo de Maner, Roehl e Williams\n")
  cat("------------------------------------------------------------------\n")

  cat("Titulo do projeto:", dados$titulo, "\n")
  cat("Nome do arquivo de saida:", dados$arqsai, "\n")
  cat(sprintf("Tempo da erosividade em anos: %10.3f\n", dados$dt))
  cat("Numero de quadriculas:", dados$NQ, "\n")
  cat("------------------------------------------------------------------\n")

  NC <- 1L
  while (NC <= dados$NQ) {
    cat("Quadricula numero: ", NC, "\n")
    cat(sprintf("Erosividade total em MJ.mm/ha/h: %10.3f\n", dados$R[NC]))
    cat(sprintf("Erodibilidade do solo em Mg.h/MJ/mm: %10.3f\n", dados$K[NC]))
    cat(sprintf("Fator LS: %10.3f\n", dados$LS[NC]))
    cat(sprintf("Fator CP: %10.3f\n", dados$CP[NC]))
    cat(sprintf("Area da quadricula em hectares: %10.3f\n", dados$AQ[NC]))
    cat("-----------------------------------------------------\n")
    NC <- NC + 1L
  }

  cat(sprintf("Area total da bacia = %10.3f ha\n", dados$AB))
  cat(sprintf("A erosao bruta total = %10.3f Mg\n", dados$ERT))
  cat(sprintf("O valor do SDR calculado = %10.3f\n", dados$SDR))
  cat(sprintf("A producao de sedimentos = %10.3f Mg/ha/ano\n", dados$PS))
  cat(sprintf("Eficiencia de retencao = %10.3f %%\n", dados$ETA))
  ASSOR <- dados$ERT * dados$ETA / 100
  cat(sprintf("Assoreamento = %10.3f Mg\n", ASSOR))
  cat("---------------------------------------------------\n")
  cat("Programa finalizado com sucesso!\n")
  readline("Pressione ENTER para sair...")
}

# ==== main ====
entrada()
calculo()
saida()
