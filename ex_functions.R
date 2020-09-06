# Tinh tien dien
tinh_tien_dien.function <- function(so_kw){
  bac1 <- 1678
  bac2 <- 1734
  bac3 <- 2014
  bac4 <- 2536
  bac5 <- 2834
  bac6 <- 2927

  tien_dien <- 0
  if (so_kw <= 50){
    tien_dien <- so_kw * bac1  
  } else if (so_kw <= 100){
    tien_dien <- (50 * bac1) + (50 * bac2)
  } else if (so_kw <=200){
    tien_dien = (50 * bac1) + (50 * bac2) + ((so_kw - 100) * bac3)
  } else if(so_kw <= 300){
    tien_dien = (50 * bac1) + (50 * bac2) + ((so_kw - 100) * bac3) + ((so_kw - 200) * bac4)
  } else if(so_kw <= 400){
    tien_dien = (50 * bac1) + (50 * bac2) + ((so_kw - 100) * bac3) + ((so_kw - 200) * bac4) + ((so_kw - 300) * bac5)
  } else {
  tien_dien = (50 * bac1) + (50 * bac2) + ((so_kw - 100) * bac3) + ((so_kw - 200) * bac4) + ((so_kw - 300) * bac5) + ((so_kw - 400) * bac6)
  }

  print(paste('Tien dien phai tra:', paste(format(tien_dien, big.mark = ',', decimal.mark = '.'), sep='')))
}

# Kiem tra so nguyen to
kt_soNT.function <- function(n){
    result <- ''
    if (n<2){
        result <- paste(n, 'is NOT PRIME!')
    } else if (n==2){
        result <- paste(n, 'is PRIME!')
    } else {
        for (i in 2:(n-1)){
            if (n%%i==0){
                result <- paste(n, 'is NOT PRIME!')
                break
            }
            result <- paste(n, 'is PRIME!')
        }
    }
    return(result)
}

# Create mode function
getmode <- function(v){
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
