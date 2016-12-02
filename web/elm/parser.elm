module Parser exposing (..)


parsedJson =
  testJson



testJson = """
[
  { "id":91541985,
    "time":"2016-10-29 01:48:04 UTC",
    "anon_visitor_id":"a86adf6b-910b-2b08-e291-c682",
    "ip_address":"76.20.48.125",
    "identity":null,
    "page":"https://trueme.goodhire.com/member/report-shared?candidateid=4402330f-4636-4323-a049-5a43643e69f9",
    "referrer":null,
    "user_agent":"Mozilla/5.0 (iPad; CPU OS 9_3_4 like Mac OS X) AppleWebKit/601.1.46 (KHTML, like Gecko) Mobile/13G35",
    "nudge_id":167540,
    "nudge_name":"Candidate Satisfaction",
    "answered_questions":
      { "321141":
        { "question_id":321141,
          "question_title":"How satisfied are you with your experience with GoodHire?",
          "question_type":"radio",
          "answer":"Very Satisfied",
          "selected_option_id":919755
        }
      }
  }
]
"""

-- ,
--   { "id":91650652,
--     "time":"2016-10-30 22:35:54 UTC",
--     "anon_visitor_id":"e05621b3-dfaa-f5a2-307c-151d",
--     "ip_address":"68.4.103.177",
--     "identity":null,
--     "page":"https://trueme.goodhire.com/member/report-shared?candidateid=fa975b06-ac88-4827-adce-6a46558b7fa2",
--     "referrer":null,
--     "user_agent":"Mozilla/5.0 (Windows NT 10.0; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/54.0.2840.71 Safari/537.36",
--     "nudge_id":167540,
--     "nudge_name":"Candidate Satisfaction",
--     "answered_questions":
--       { "321141":
--         { "question_id":321141,
--           "question_title":"How satisfied are you with your experience with GoodHire?",
--           "question_type":"radio",
--           "answer":"Very Satisfied",
--           "selected_option_id":919755
--         }
--       }
--     }
-- ]
