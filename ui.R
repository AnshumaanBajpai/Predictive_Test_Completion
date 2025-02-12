library(shiny)

shinyUI(navbarPage("Data Science Specialization Capstone",
                   tabPanel("Predict Next Word",
                            sidebarLayout(position = "left",
                                    sidebarPanel(h3("Input and Controls", style = "color:red"),
                                                 p(strong("Please read the 'Help?' tab before using this application.")),
                                                 textInput(inputId = "inp_str", label = "Enter your string", value = ""),
                                                 br(),
                                                 p("Select the number of predictions that you wish"),
                                                 radioButtons("n_pred", label = h3("No. of predictions"),
                                                                          choices = list(1, 2, 3, 4, 5), 
                                                                          selected = 3),
                                                 submitButton(text = "Submit", icon = icon("thumbs-up"))),
                                    mainPanel(
                                            tabsetPanel(type = "tabs",
                                                        tabPanel("Predictions",
                                                                 h3("Predictions", style = "color:green", align = "center"),
                                                                 h4("The details entered by you are:"),
                                                                 p("String:", verbatimTextOutput("out_str")),
                                                                 p("No. of predictions:", verbatimTextOutput("n_pred")),
                                                                 br(),
                                                                 h4("The top predictions in the decreasing order of probability for the next word are:"),
                                                                 verbatimTextOutput("prediction")),
                                                        tabPanel("Model Details",
                                                                 h3("Details", style = "color:green", align = "center"),
                                                                 br(),
                                                                 p("1. The model uses 10% of the data from the text files provided in en_US directory."),
                                                                 p("2. Only n-grams up to 4-grams have been considered for word prediction."),
                                                                 p("3. Attempt has been made to clean all the random punctuation marks that probably are meant to reperesent emoticons and are replaced by the term '<emoticons>'."),
                                                                 p("4. All the numbers followed by '$' sign have been replaced by '<dollaramount>'. Other occurences of numbers have been replaced by '<num>'"),
                                                                 p("5. '#' has been replaced by the term 'hashtag'"),
                                                                 p("6. The model uses the profane words from the song '7 dirty words' as a guideline to tag occurences of those words as '<profanity>'."),
                                                                 p("7. For Uni-grams, all possible occurences have been taken into consideration. For higher order n-grams, only the phrases with frequency 2 or more have been considered to meet the size requirements for shiny"),
                                                                 p("8. All the codes for this work can be accessed at the following ",
                                                                   a("GitHub.", target = "_blank", href = "https://www.linkedin.com/pub/anshumaan-bajpai/19/781/7a1"))
                                                                 ),
                                                        tabPanel("Limitations",
                                                                 h3("Limitations", style = "color:green", align = "center"),
                                                                 br(),
                                                                 p("1. The Unigram tokenizer used here does not account for the single letter words so it may have issues if the word before the prediction is 'a' or any other single letter word."),
                                                                 p("2. The model predicts the most frequent unigrams if the entered string is empty"),
                                                                 p("3. The text cleaning isn't perfect. Inspite of my attempts to clear all the '$' and other special characters, there are many which are still there because they didn't match the strings that I looked for."),
                                                                 p("4. I tried implementing Kneser-Ney for prediction but kept running into some bugs which I could not fix and therefore the project uses only Stupid Back-Off for prediction.")),
                                                        tabPanel("Acknowledgements",
                                                                 h4("I would like to acknowlednge the contributions of the following resources that helped me in building this project."),
                                                                 br(),
                                                                 h5(a("Coursera Discussion Board", target = "_blank", href = "https://class.coursera.org/dsscapstone-004/forum")),
                                                                 h5(a("The tm package", target = "_blank", href = "https://cran.r-project.org/web/packages/tm/tm.pdf")),
                                                                 h5(a("Introduction to NLP", target = "_blank", href = "https://class.coursera.org/nlp/lecture/preview")),
                                                                 h5(a("Stack Overflow", target = "_blank", href = "http://stackoverflow.com/")),
                                                                 h5("And of course,"),
                                                                 h5(a("Google", target = "_blank", href = "https://www.google.com/?gws_rd=ssl")),
                                                                 h5("I would specifically like to thank the course TAs and other colleauges for the priceless discssions in the Discussion forum."))
                                                        )
                                            ))),
                   tabPanel("Help ?",
                            sidebarLayout(position = "right",
                                    sidebarPanel(h3("About me:"),
                                                 p("Ahoy!,"),
                                                 p("I am Anshumaan Bajpai, a graduate student in the Department of Chemical and Biomolecular Engineering at University of Notre Dame, USA."),
                                                 p("I hope you enjoyed using this app. In case you have any questions, please feel free to drop me an email at ",
                                                   span(a("bajpai.anshumaan@gmail.com", href="mailto:bajpai.anshumaan@gmail.com"))),
                                                 p("To know more about my technical background and career interests, please visit my LinkedIn page ",
                                                   span(a("here", target = "_blank", href = "https://www.linkedin.com/pub/anshumaan-bajpai/19/781/7a1")),
                                                   " .")),
                                    mainPanel(h2("About the App:"),
                                              br(),
                                              p("This app attempts to predict the next word for a given incomplete string."),
                                              p("The application uses three kinds of data sets (twitter, blogs and news) to build the prediction algorithm."),
                                              p("The app uses stupid Back-Off smoothing algorithm for prediction. I tried implementing Kenser Ney but for some reason kept running into bugs."),
                                              p("It allows the user to select the number of words that he/she wants to see and the results are displayed in the decreasing order of the probability"),
                                              p("A detailed discription of this project is available as an ",
                                                a("R presentation.", target = "_blank", href = "http://rpubs.com/AnshumaanBajpai/ATP")))
                            ))
))