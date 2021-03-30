curl --location --request PUT 'https://sheets.googleapis.com/v4/spreadsheets/1nCeGC0XHnIVwQ3EyGRMZRmExkiP_wpVgotIZQS98e28/values/Sheet1!E2:E2?valueInputOption=USER_ENTERED' \
--header 'Authorization: Bearer ya29.c.Kp8B-QcXhPUoRQut5_oj0TjZHjj0dJM5OlS3ub8vxI4gGftVZRiQaIoYMJRoU7s9ZnbjpY-tsx96MNhuKJJznWnePIZXkk82IAwFYQOywefiSkLKtYHgdOHv94RsHOl93MTBI4oSRkfkxKUdnCnCTaXycuWrnvQu-4RWJuPf0v3tiPpcKuxVlrnj5Nsmt0lWH3pQEas2g_1CkFqAFOtneKT1' \
--header 'Content-Type: application/json' \
--data-raw '        {
          "range": "Sheet1!E2:E2",
          "majorDimension": "ROWS",
          "values": [
            [
              "85000"
            ]
          ]
        }'



curl --location --request GET 'https://sheets.googleapis.com/v4/spreadsheets/1nCeGC0XHnIVwQ3EyGRMZRmExkiP_wpVgotIZQS98e28/values/Sheet1!B5:E5' \
--header 'Authorization: Bearer ya29.c.Kp8B-QcXhPUoRQut5_oj0TjZHjj0dJM5OlS3ub8vxI4gGftVZRiQaIoYMJRoU7s9ZnbjpY-tsx96MNhuKJJznWnePIZXkk82IAwFYQOywefiSkLKtYHgdOHv94RsHOl93MTBI4oSRkfkxKUdnCnCTaXycuWrnvQu-4RWJuPf0v3tiPpcKuxVlrnj5Nsmt0lWH3pQEas2g_1CkFqAFOtneKT1'
