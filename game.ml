open UniverseJs
open Color
open Image
open World
open TransformToInt

type world_t = {
  apple : int * int;
  banana : int * int;
  apple2 : int * int;
  banana2 : int * int;
  apple3 : int * int;
  banana3 : int * int;
  gf : int * int;
  ichigo : int * int;
  score : int;
  mouse : bool;
  mouseb : bool;
  mousea2 : bool;
  mouseb2 : bool;
  mousea3 : bool;
  mouseb3 : bool;
  time_score : int * int * int;
  mi_score : int * int * int;
}
let initial_world = {apple = (100, 50);banana = (200, 50);
                     apple2 = (180, 560);banana2 = (400, 560);
                     apple3 = (360, 50);banana3 = (60, 560);gf = (280, 50);ichigo = (500, 560);
                     score = 0;
                     mouse = false;
                     mouseb = false;
                     mousea2 = false;
                     mouseb2 = false;
                     mousea3 = false;
                     mouseb3 = false;
                     time_score = (0,-50,-50);mi_score = (0, -50, -50)}
let width = 560
let height = 560
let apple = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/apple.png" 100 100
let banana = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/banana.png" 100 100
let apple2 = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/apple.png" 100 100
let banana2 = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/banana.png" 100 100
let apple3 = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/apple.png" 100 100
let banana3 = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/banana.png" 100 100
let gf = read_image "images/grapefruits.png" 100 100
let ichigo = read_image "images/suika.png" 100 100
let background = read_image "http://pllab.is.ocha.ac.jp/~asai/picture/images/background.png" 560 560
    
let draw {apple = (x, y); banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
          apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
          score = score;
          mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
          time_score = (s, sx, sy); mi_score = (ms, msx, msy)} =

  (* if score > -1  then  *)
  place_image (text (string_of_int score) 60 Color.slateGray) (60, 60)
    (place_image (text ("+"^string_of_int s) 20 Color.black) (sx, sy)
    (place_image (text ("-"^string_of_int ms) 20 Color.black) (msx, msy)
    (place_image apple (x, y)
       (place_image banana (bx, by)
       (place_image apple2 (a2x, a2y)
       (place_image banana2 (b2x, b2y)
       (place_image apple3 (a3x, a3y)
       (place_image banana3 (b3x, b3y)
       (place_image gf (gx, gy)
       (place_image ichigo (ix, iy)
       (place_image (rectangle 200 200 salmon) (460,280)
       (place_image (rectangle 200 200 yellow) (100,280)
          (place_image background (280, 280) (empty_scene width height))))))))))))))

  (* else
   *   place_image (text ("Game Over") 20 Color.black) (280, 280)
   *   (place_image (rectangle 500 500 dimGray) (280,280)
   *     (place_image background (280, 280)(empty_scene width height))) *)


(* up_fruit : int*int -> int *)
let up_fruit (x, y) =
  if y > 570  then ((Random.int 500)+50 ,0)  else (x , y)

(* up_fruit4 : int*int -> int *)
let up_fruit4 (x, y) =
  if y <= -10  then ((Random.int 500)+50, 560)  else (x , y)

(* score_minus :  int*int ->int*int -> int -> int *)
let score_minus (x, y) (bx, by) (a2x, a2y) (b2x, b2y) (a3x, a3y) (b3x, b3y) score =
  if ((y >= 560)&&(y < 565)) || ((by >= 560)&&(by <= 566)) ||
     ((a2y <= 0)&&(-5 <= a2y)) || ((b2y <= 0)&&(-6 <= b2y))||
     ((a3y >= 560)&&(a3y < 565)) || ((b3y <= 0)&&(-6 <= b3y))
  then score-10  else score




let on_tick {apple = (x, y); banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
             apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
             score = score;
             mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
             time_score = (s, sx, sy); mi_score = (ms, msx, msy)} =
  {apple = up_fruit (x, y + 5);
   banana = up_fruit (bx, by + 6);
   apple2 = up_fruit4 (a2x, a2y - 5);
   banana2 = up_fruit4 (b2x, b2y - 6);
   apple3 = up_fruit (a3x, a3y + 5);
   banana3 = up_fruit4 (b3x, b3y - 6);
   gf = up_fruit (gx, gy + 5);
   ichigo = up_fruit4 (ix, iy - 5);
   score = score_minus (x, y) (bx, by) (a2x, a2y) (b2x, b2y) (a3x, a3y) (b3x, b3y) score;
   mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
   time_score = (s, sx, sy); mi_score = (ms, msx, msy)}
  
let on_mouse {apple = (x, y); banana = (bx, by);score = score;
              apple2 = (a2x, a2y); banana2 = (b2x, b2y);
              apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
              mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
              time_score = (s, sx, sy); mi_score = (ms, msx, msy)} mouse_x mouse_y event =
  (* りんご掴む *)
  if (event = "button_down") && (x-50 < mouse_x) && (mouse_x < x+50)&&(y-50 < mouse_y) && (mouse_y < y+50)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = true; mouseb = mouseb;mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, -50, -50); mi_score = (ms,-50, -50)}
  else if (event = "button_up") && (mouse = true) && (400 < mouse_x) && (mouse_x < 560) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = ((Random.int 500)+50,-50);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score + 10;
     mouse = false; mouseb = mouseb;mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (10,mouse_x+10,mouse_y+10); mi_score = (ms, msx, msy)}
  else if (event = "button_up") && (mouse = true) && (0 < mouse_x) && (mouse_x < 180) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = ((Random.int 500)+50,-50);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score - 20;
     mouse = false; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (20, mouse_x+10,mouse_y+10)}
  else if (event = "button_up") && (mouse = true)
  then
    {apple = (mouse_x,mouse_y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = false; mouseb = mouseb;  mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (ms, msx, msy)}

  (* りんご2掴む *)
  else  if (event = "button_down") && (a2x-50 < mouse_x) && (mouse_x < a2x+50)&&(a2y-50 < mouse_y) && (mouse_y < a2y+50)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb;  mousea2 = true; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, -50, -50); mi_score = (ms,-50, -50)}
  else if (event = "button_up") && (mousea2 = true) && (400 < mouse_x) && (mouse_x < 560) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by); apple2 = ((Random.int 500)+50,650); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score + 10;
     mouse = mouse; mouseb = mouseb;  mousea2 = false; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (10,mouse_x+10,mouse_y+10); mi_score = (ms, msx, msy)}
  else if (event = "button_up") && (mousea2 = true) && (0 < mouse_x) && (mouse_x < 180) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by);apple2 = ((Random.int 500)+50,650); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score - 20;
     mouse = mouse; mouseb = mouseb; mousea2 = false; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (20, mouse_x+10,mouse_y+10)}
  else if (event = "button_up") && (mousea2 = true)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (mouse_x,mouse_y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb; mousea2 = false; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (ms, msx, msy)}


      (* りんご3掴む *)
  else if (event = "button_down") && (a3x-50 < mouse_x) && (mouse_x < a3x+50)&&(a3y-50 < mouse_y) && (mouse_y < a3y+50)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb;mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = true; mouseb3 = mouseb3;
     time_score = (s, -50, -50); mi_score = (ms,-50, -50)}
  else if (event = "button_up") && (mousea3 = true) && (400 < mouse_x) && (mouse_x < 560) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = ((Random.int 500)+50,-50); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score + 10;
     mouse = mouse; mouseb = mouseb;mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = false; mouseb3 = mouseb3;
     time_score = (10,mouse_x+10,mouse_y+10); mi_score = (ms, msx, msy)}
  else if (event = "button_up") && (mousea3 = true) && (0 < mouse_x) && (mouse_x < 180) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = ((Random.int 500)+50,-50); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score - 20;
     mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = false; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (20, mouse_x+10,mouse_y+10)}
  else if (event = "button_up") && (mousea3 = true)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (mouse_x,mouse_y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb;  mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = false; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (ms, msx, msy)}
    
  (* ばなな掴む *)
  else  if (event = "button_down") && (bx-50 < mouse_x) && (mouse_x < bx+50)&&(by-50 < mouse_y) && (mouse_y < by+50)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y); mousea3 = mousea3; mouseb3 = mouseb3;
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = false; mouseb = true; mousea2 = mousea2; mouseb2 = mouseb2;
     time_score = (s, -50, -50); mi_score = (ms,-50, -50)}
  else if (event = "button_up") && (mouseb = true) && (0 < mouse_x) && (mouse_x < 180) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = ((Random.int 500)+50,-50);apple2 = (a2x, a2y); banana2 = (b2x, b2y); mousea3 = mousea3; mouseb3 = mouseb3;
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score + 10;
     mouse = mouse; mouseb = false; mousea2 = mousea2; mouseb2 = mouseb2;
     time_score = (10,mouse_x+10,mouse_y+10); mi_score = (ms, msx, msy)}
  else if (event = "button_up") && (mouseb = true) && (400 < mouse_x) && (mouse_x < 560) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = ((Random.int 500)+50,-50);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score - 20;
     mouse = mouse; mouseb = false; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (20, mouse_x+10,mouse_y+10)}
  else if (event = "button_up") && (mouseb = true)
  then
    {apple = (x, y);banana = (mouse_x,mouse_y); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = false;  mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (ms, msx, msy)}

 (* ばなな2掴む *)
  else  if (event = "button_down") && (b2x-50 < mouse_x) && (mouse_x < b2x+50)&&(b2y-50 < mouse_y) && (mouse_y < b2y+50)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb;  mousea2 = mousea2 ; mouseb2 =true; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, -50, -50); mi_score = (ms,-50, -50)}
  else if (event = "button_up") && (mouseb2 = true) && (0 < mouse_x) && (mouse_x < 180) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = ((Random.int 500)+50,650);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score + 10;
     mouse = mouse; mouseb = mouseb;  mousea2 = mousea2; mouseb2 = false; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (10,mouse_x+10,mouse_y+10); mi_score = (ms, msx, msy)}
  else if (event = "button_up") && (mouseb2 = true) && (400 < mouse_x) && (mouse_x < 560) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = ((Random.int 500)+50,650);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score - 20;
     mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = false; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (20, mouse_x+10,mouse_y+10)}
  else if (event = "button_up") && (mouseb2 = true)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (mouse_x, mouse_y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = false; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, sx, sy); mi_score = (ms, msx, msy)}


  (* ばなな3掴む *)
  else  if (event = "button_down") && (b3x-50 < mouse_x) && (mouse_x < b3x+50)&&(b3y-50 < mouse_y) && (mouse_y < b3y+50)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb;  mousea2 = mousea2 ; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = true;
     time_score = (s, -50, -50); mi_score = (ms,-50, -50)}
  else if (event = "button_up") && (mouseb3 = true) && (0 < mouse_x) && (mouse_x < 180) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = ((Random.int 500)+50,650); gf = (gx, gy); ichigo = (ix, iy);
     score = score + 10;
     mouse = mouse; mouseb = mouseb;  mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = false;
     time_score = (10,mouse_x+10,mouse_y+10); mi_score = (ms, msx, msy)}
  else if (event = "button_up") && (mouseb3 = true) && (400 < mouse_x) && (mouse_x < 560) && (180 < mouse_y) && (mouse_y < 380)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = ((Random.int 500)+50,650); gf = (gx, gy); ichigo = (ix, iy);
     score = score - 20;
     mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = false;
     time_score = (s, sx, sy); mi_score = (20, mouse_x+10,mouse_y+10)}
  else if (event = "button_up") && (mouseb3 = true)
  then
    {apple = (x, y);banana = (bx, by);apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (mouse_x, mouse_y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = false;
     time_score = (s, sx, sy); mi_score = (ms, msx, msy)}
    
  else
    {apple = (x, y);banana = (bx, by); apple2 = (a2x, a2y); banana2 = (b2x, b2y);
     apple3 = (a3x, a3y); banana3 = (b3x, b3y); gf = (gx, gy); ichigo = (ix, iy);
     score = score;
     mouse = mouse; mouseb = mouseb; mousea2 = mousea2; mouseb2 = mouseb2; mousea3 = mousea3; mouseb3 = mouseb3;
     time_score = (s, -50, -50); mi_score = (ms, -50, -50)}
    
;; big_bang initial_world
  ~width:width
  ~height:height
  ~to_draw:draw
  ~on_tick:on_tick
  ~on_mouse:on_mouse
  ~rate:80
