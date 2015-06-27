library(devtools)
install_github("rpremraj/mailR")

library(mailR)
library(openxlsx)
library(knitr)


price <- c(Milk = 3.50,
			Cream = 6.00,
			Yogurt = 4.00,
			Whey = 2.00,
			MediumEggs = 2.50,
			LargeEggs = 3.50,
			OtherCategory = 0)


spread_sheet <- read.xlsx("purchase_records.xlsx", sheet=1, startRow=1, colNames=TRUE, detectDates=T)




# Here we want to get the names of everyone that purchased something this month.
# Eventually, we'll want to double check that we have an email address for
# everyone and that we haven't introduced a typo into the spreadsheet.

patrons <- levels(factor(spread_sheet$Name))

make_email <- function(surname){
	breslin <- spread_sheet[spread_sheet$Name==surname,]
	patron <- breslin[1,2]
	dates <- breslin[,1]
	returns <- breslin[,3]
	breslin <- breslin[,-c(1,2,3)]

	n_entries <- sum(!is.na(breslin[,-ncol(breslin)]))
	invoice_table <- data.frame(Date=rep("xxx", n_entries),
								Items=rep("xxx", n_entries),
								Quantity=rep(0, n_entries),
								Price=rep(0, n_entries),
								Total=rep(0, n_entries),
								stringsAsFactors=FALSE
								)

	counter <- 1
	for(visit in 1:nrow(breslin)){
		for(category in names(price)){
			if(!is.na(breslin[visit,category])){

				invoice_table[counter, "Date"] <- as.character(dates[visit])

				if(category != "OtherCategory"){
					invoice_table[counter, "Items"] <- category
					invoice_table[counter, "Quantity"] <- breslin[visit,category]
					invoice_table[counter, "Price"] <- price[category]
					invoice_table[counter, "Total"] <- price[category] * breslin[visit,category]
				} else {
					invoice_table[counter, "Items"] <- breslin[visit,category]
					invoice_table[counter, "Quantity"] <- 1
					invoice_table[counter, "Price"] <- breslin[visit,"OtherPrice"]
					invoice_table[counter, "Total"] <- breslin[visit,"OtherPrice"]
				}
				counter <- counter + 1
			}

		}
	}
	invoice_table$Price <- format(invoice_table$Price, nsmall=2L)
	invoice_table$Total <- format(invoice_table$Total, nsmall=2L)

	pretty_table <- kable(invoice_table, format="pandoc", align=c('l', 'l', 'c', 'r', 'r'))
	grand_total <- sum(as.numeric(invoice_table$Total))

	text <- paste0("Your total for the month of ", months(dates[1]), " is $", format(grand_total, nsmall=2L), ". The details of what items you obtained from Eight Kids Farm are in the table below. You can pay by cash or check the next time you visit. If you have questions about your bill, please email me at sjschloss@gmail.com or call (734)424-0331. We appreciate your support of our farm as we strive to provide our community with healthy food while raising our family.")

	body <- paste(c(text, "", "", pretty_table), collapse="\n")

	send.mail(from = "sjschloss@gmail.com",
          to = c("pdschloss@gmail.com"),
          subject = "Subject of the email",
          body = body,
          smtp = list(host.name = "aspmx.l.google.com", port = 25),
          authenticate = FALSE,
		  encoding = "us-ascii",
          send = TRUE)

}

sapply(patrons, make_email)
