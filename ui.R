library(shiny)
library(shinydashboard)
library(googlesheets)
library(dplyr)
library(DT)

# Notification menu
notifications <- dropdownMenu(
                              
)

# Header menu
header <- dashboardHeader(title = "Visibility Tool Usability", notifications)

# Sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
    menuItemOutput("menuitem")
  )
)

# Body
body <- dashboardBody(
  #uiOutput("tabs")
  tabItems(
    tabItem("LL",uiOutput("LLview"),
            actionButton("LLcounter", "Submit")),
    tabItem("LH", uiOutput("LHtable"),
            actionButton("LHcounter", "Submit")),
    tabItem("ML", uiOutput("MLview"),
            actionButton("MLcounter", "Submit")),
    tabItem("MH", uiOutput("MHtable"),
            actionButton("MHcounter", "Submit")),
    tabItem("HL", uiOutput("HLview"),
            actionButton("HLcounter", "Submit")),
    tabItem("HH", uiOutput("HHtable"),
            actionButton("HHcounter", "Submit")),
    tabItem("lp", 
            fluidPage(
            column(width=10,
                   offset = 1,
              box(width=12, height = "100%",
                  solidHeader = FALSE,
                  withTags({
                    div(style="overflow-y: scroll;height:80vh;",
                      h1("DMDII - Visibility Tool Usability Study", align="center"),
                      p("We are asking you to be in a research study. The purpose of this consent form is to give you the information you will need to help you decide whether to be in the study or not. Please read the form carefully. You may ask questions about the purpose of the research, what we would ask you to do, the possible risks and benefits, your rights as a volunteer, and anything else about the research or this form that is not clear. When we have answered all your questions, you can decide if you want to be in the study or not. This process is called informed consent We will give you a copy of this form for your records."),
                      h2("Purpose of the Study", align="center"),
                      p("The objective of the current study is to assess the users' use and acceptance of the visibility tool assuming that the real-time information is accurate and reliable. This research will help to make sure that user interface is designed to maximize the understanding and use of the part flow information as well as the overall system usefulness."),
                      h2("Study Procedures", align="center"),
                      p("Approximately 60 people will take part in this study remotely at their respective work location. Your involvement in this study will consist of using an web app for approximately 30-40 minutes. There may be additional contact from the study team for a follow up study."),
                      p("Your written consent will be obtained upon using the web app. You may email us any questions you have."),
                      p("When using the usability web app, you will be asked to complete a questionnaire that covers some general questions relating to your current position and demographics. For example, the questions will ask you how much experience do you have at your current role?"),
                      p("First, you will receive instructions on how to operate the visibility tool and you will be asked to complete a practice scenario that is about 5 minutes long, so that you can become familiar with the visibility tool."),
                      p("The study will follow this same procedure, which are as follows:"),
                      ul(
                        li("Read informed consent"),
                        li("Practice navigating the visbility tool"),
                        li("Practice scenario for visibility tool"),
                        li("Main Evaluation of visibility tool and survey")
                      ),
                      p("You may skip any questions that you do not wish to answer on the surveys. All evaluation studies will be screen captured. The system contains a screen capture software that will capture all the interactions of the study subject and the visibility tool."),
                      h2("RISKS, STRESS, OR DISCOMFORT", align="center"),
                      h2("DMDII - Visibility Tool Evaluation", align="center"),
                      p("There may be some risks from being in this study. Despite limiting the study to a short period of time, you may feel discomfort associated with eye soreness and dryness associated with extended computer use. These feelings were usually mild and consisted of slight dryness and soreness of the eyes. These effects typically last for only a short time, usually 10-15 minutes, after leaving the visibility tool. If you become uncomfortable at any time during your participation, please inform the experimenter immediately."),
                      p("In case of a breach of confidentially, your survey responses and screen capture data could be accessed. To minimize this impact, your name will not be directly associated with this data, and the data will be securely stored in a password protected computer in a limited access room."),
                      h2("BENEFITS OF THE STUDY", align="center"),
                      p("There are no direct benefits to participants in the study. However, we hope that information from this study will help us to evaluate the effects of the visibility tool and the presentation of the data in decision making. Feedback provided by the participants will help to shape the design of future iterations of the visibility tool."),
                      h2("CONFIDENTIALITY OF RESEARCH INFORMATION", align="center"),
                      p("Your confidentiality will be protected throughout data collection and analysis. This will be accomplished by assigning a study number to you, for which your data will be identified by. All the data collected from this study will be represented by this number; your name will not be directly associated with your questionnaire answers, screen capture data or survey data, but will be linked through by a study code. This link between you as a subject and the study number will be maintained until the study is complete and all records retention requirements have passed, at which point it will be destroyed."),
                      p("The experimental results, and survey results will be kept on a password secure device and access will be given only to this research team at University of Washington."),
                      p("Government or university staff sometimes review studies such as this one to make sure they are being done safely and legally.  If a review of this study takes place, your records may be examined. The reviewers will protect your privacy. The study records will not be used to put you at legal risk of harm."),
                      p("We will keep your participation in this research study confidential to the extent permitted by law.  To help protect your confidentiality, you will be assigned a study number that will be used instead of your name to identify all data collected for the study. All information you provide will be confidential. However, if we learn that you intend to harm yourself or others, we must report that to the authorities."),
                      h2("OTHER INFORMATION", align="center"),
                      p("You may refuse to participate, and you are free to withdraw from this study at any time without penalty or loss of benefits to which you are otherwise entitled. The data collected from the participant surveys will be collectively analyzed. The findings from this research may be publicly released in final reports or other publications for scientific, educational, outreach, or research purposes."),
                      p("Your participation in this study is entirely voluntary, and you are free to withdraw from the study at any time."),
                      h2("RESEARCH-RELATED INJURY", align="center"),
                      p("If you think you have a medical problem or illness related to this research, contact study staff Steven Hwang at hwang216@uw.edu."),
                      h3("Subject's statement"),
                      p("This study has been explained to me.  I volunteer to take part in this research.  I have had a chance to ask questions.  If I have questions later about the research, or if I have been harmed by participating in this study, I can contact one of the researchers listed on the first page of this consent form.  If I have questions about my rights as a research subject, I can call the Human Subjects Division at (206) 543-0098 or call collect at (206) 221-5940.")
                    )#End Div
                  }),
                  textInput("signature", "Signature:"),
                  actionButton("sigSubmit", "Submit")
              )
            )
            )),
    tabItem("ps", h1("D was done"))
  )
)

dashboardPage(header, sidebar, body, skin = "purple")