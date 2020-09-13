library(data.table)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggsci)

d <- fread("SNA_TABLE5.csv")

d.us <- filter(d, LOCATION == "USA", MEASURE == "VOB");

transact.map <- filter(d.us, Year == 2008) %>% select(TRANSACT, Transaction);
transact.map <- transact.map[order(transact.map$TRANSACT), ];

print(with(transact.map, cbind(TRANSACT, substring(Transaction, 1, 50))))


recessions <- data.frame(
	start = c(1973, 1980, 1990, 2001-0.5, 2007),
	end = c(1975, 1982, 1991, 2001, 2009)
);

annotate_recessions <- function(recessions) {
	annotate("rect",
		ymin = -Inf, ymax = Inf,
		xmin = recessions$start, xmax = recessions$end,
		fill="grey90"
	)
}

# Overall domestic consumption expenditure
ggplot(filter(d.us, TRANSACT %in% c("P31DC")), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Broad categories
transacts.broad <- c("P311B", "P312B", "P313B", "P314B");
ggplot(filter(d.us, TRANSACT %in% transacts.broad), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Food
transact <- c("P31CP010", "P31CP011", "P31CP012");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Alocoholic beverages, tobacco and narcotics
transact <- c("P31CP020", "P31CP021", "P31CP022");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Clothing and footwear
transact <- c("P31CP030", "P31CP031", "P31CP032");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Housting, water, electricity, gas, and other fuels
#transact <- c("P31CP040", "P31CP041", "P31CP042", "P31CP044", "P31CP045")
transact <- c("P31CP041", "P31CP042", "P31CP044", "P31CP045")
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Furnishings, households equipment and routine main
transact <- c("P31CP050", "P31CP051", "P31CP052", "P31CP053", "P31CP054", "P31CP055", "P31CP056");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Health
transact <- c("P31CP060", "P31CP061", "P31CP062", "P31CP063");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Transport
transact <- c("P31CP070", "P31CP071", "P31CP072", "P31CP073");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Communications
transact <- c("P31CP080", "P31CP081");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Recreation and culture
transact <- c("P31CP090", "P31CP091", "P31CP092", "P31CP093", "P31CP094", "P31CP095", "P31CP096" );
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Education
transact <- c("P31CP100", "P31CP101");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Restaurants and hotels
transact <- c("P31CP110", "P31CP111", "P31CP112");
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

# Miscellaneous goods and services
transact <- c("P31CP120", "P31CP121", "P31CP122_127", "P31CP123", "P31CP124", "P31CP125", "P31CP126", "P31CP127")
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom") +
	scale_colour_npg()

transact <- c("P31CP061")  # medical products
transact <- c("P31CP045")  # electricity
ggplot(filter(d.us, TRANSACT %in% transact), aes(x=Year, y=Value, colour=Transaction)) +
	annotate_recessions(recessions) +
	geom_line() + theme_classic() +
	theme(legend.position = "bottom")

# Flat:
#   medical products, appliances and equipment *
#   electricity (but business electricity consumption is down)
#   accomodation services (but current recession is caused by pandemic)
# Increase:
#   communications *
#   audio-visual, photographic and information processing equipment (i.e. mobile phones)
#   social protection (but service is provided by government)
#   hospital services (increased price?)
#   out-patient services (increased price?)
#   rentals for housing (increased price)
# Decrease:
#   everything else, including water supply and personal care

