open! Bonsai_web

let () = Inline_css.Private.append "/* direct from lib */"
let app = "hi there"

module Style =
  [%css.raw
    {| 
   .foo #foo #bar {
       background: 5;
       red: blue;
   }
|}]
