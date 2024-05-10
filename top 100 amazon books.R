  Url1<-"https://raw.githubusercontent.com/daladnoo1/Rstudio-top100books/master/Books.csv"
  Books<-read.csv(Url1)
  Books
  Url2<-"https://raw.githubusercontent.com/daladnoo1/Rstudio-top100books/master/Price2.csv"
  price<-read.csv(Url2)
  price
   Books$year <- as.numeric(Books$year)
  Books
  library(ggplot2)
  ggplot(Books, aes(x = price)) +
    geom_histogram(binwidth = 6, fill = "blue", color = "black") +
    labs(title = "Distribution of Prices", x = "Price", y = "Frequency")
  
  
  library(lattice)
  
  densityplot(~ price, data = Books, main = "Density Plot of Prices")
  #############
  library(ggplot2)
  Books
  ##### from 0 to 25 price
  ggplot(Books, aes(x = price, y = as.numeric(gsub(",", "", no_of_reviews)))) +
    geom_point(color = "red", alpha = 0.7) +
    labs(title = "Scatter Plot of Prices and Number of Reviews", x = "Price", y = "Number of Reviews")
  
  
  
  
  # Distribution of Ratings
  ggplot(Books, aes(x = ratings)) +
    geom_histogram(binwidth = 0.05, fill = "purple", color = "black", alpha = 0.7) +
    labs(title = "Distribution of Ratings", x = "Rating", y = "Frequency")
  
  ###Price by years from 2013 to 2015 were the biggest prices
  average_prices <- Books %>%
    group_by(year) %>%
    summarise(mean_price = mean(price, na.rm = TRUE))
  ggplot(average_prices, aes(x = year, y = mean_price)) +
    geom_line(color = "blue", alpha = 0.7) +
    labs(title = "Average Book Prices Over the Years", x = "Year", y = "Average Price")

  
  library(ggplot2)
  library(dplyr)
  
  grouped_books <- group_by(Books, author)
  summarized_books <- summarise(grouped_books, total_books = n_distinct(title))
  top_authors_books <- top_n(summarized_books, 10, total_books)
  
  ggplot(top_authors_books, aes(x = reorder(author, -total_books), y = total_books, fill = author)) +
    geom_col() +
    labs(title = "Top 10 Authors with Most Number of Books",
         x = "Author", y = "Total Number of Books") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_y_continuous(breaks = seq(0, max(top_authors_books$total_books), by = 2))
  
  
  ############
  library(ggplot2)
  library(dplyr)
  
  
  
  Books$ranks <- as.numeric(Books$ranks)
  
  grouped_books <- group_by(Books, year)
  min_rank_books <- slice_min(grouped_books, order_by = ranks)
  top_books <- mutate(min_rank_books, top_selling_book = title)
  
 
  grouped_top_authors <- group_by(top_books, author, title)
  author_counts <- summarize(grouped_top_authors, count = n())
  
 
  ggplot(author_counts, aes(x = author, y = count, fill = title)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Authors who have been in the top for the most years", x = "Author", y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ##############
  library(dplyr)
  
  
  ggplot(price, aes(x = Author.s., y = Approximate.sales.in.millions, fill = Book)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Best selling books", x = "Author", y = "Approximate sales in millions") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ##########
  
  
  top_expensive_books <- head(arrange(Books, desc(price)), n = 7) %>%
    distinct(title, .keep_all = TRUE)
  
  ggplot(top_expensive_books, aes(x = reorder(title, -price), y = price, fill = title)) +
    geom_col() +
    labs(title = "Top 5 Most Expensive Books",
         x = "Book Title", y = "Price") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")
  
##########
  ratings_by_year <- Books %>% select(year, ratings)
  
  
  average_rating_by_year <- ratings_by_year %>%
    group_by(year) %>%
    summarise(mean_ratings = mean(ratings, na.rm = TRUE))
  

  ggplot(average_rating_by_year, aes(x = year, y = mean_ratings)) +
    geom_line() +
    labs(title = 'Average Ratings By Year', x = 'Year', y = 'Average Ratings') +
    theme_minimal()
      ############
  
  
  cover_counts <- Books %>%
    group_by(cover_type) %>%
    summarise(book_count = n())
  ggplot(cover_counts, aes(x = cover_type, y = book_count, fill = cover_type)) +
    geom_bar(stat = "identity") +
    labs(title = "Number of Books by Cover Type",
         x = "Cover Type",
         y = "Number of Books") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  
  