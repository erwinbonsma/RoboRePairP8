pico-8 cartridge // http://www.pico-8.com
version 23
__lua__
-- bumble bots re-pair
-- (c) 2020, erwin bonsma

fps=60
vector={}

function vector:new(x,y)
 local o=setmetatable({},self)
 self.__index=self

 o.x=x
 o.y=y

 return o
end

function vector:__add(v)
 return vector:new(
  self.x+v.x,self.y+v.y
 )
end

function vector:__mul(f)
 return vector:new(
  self.x*f,self.y*f
 )
end

function vector:__eq(v)
 return (
  self.x==v.x and
  self.y==v.y
 )
end

function vector:add(v)
 self.x+=v.x
 self.y+=v.y
end

function vector:sub(v)
 self.x-=v.x
 self.y-=v.y
end

function vector:lerp(v,f)
 self.x=(1-f)*self.x+f*v.x
 self.y=(1-f)*self.y+f*v.y
end

function vector:dist(v)
 local dx=self.x-v.x
 local dy=self.y-v.y
 return sqrt(dx*dx+dy*dy)
end

function vector:to_string()
 return "("..self.x..","..self.y..")"
end

function vector:orientation(vref)
 local val=(
  self.y*vref.x-self.x*vref.y
 )
 if val==0 then
  return 0  --co-linear
 elseif val>0 then
  return 1  --clockwise
 else
  return -1 --counter-clockwise
 end
end

dirs={1,2,3,4}
vdirs={
 vector:new(0,-1),
 vector:new(1,0),
 vector:new(0,1),
 vector:new(-1,0)
}

function clkwise(dr)
 return 1+dr%4
end

function cclkwise(dr)
 return 1+(dr+2)%4
end

function opposite(dr)
 return 1+(dr+1)%4
end

function dump_list(l)
 local s="["
 for i in all(l) do
  if #s>1 then
   s=s..","
  end
  s=s..i
 end
 s=s.."]"
 printh(s)
end

function rnd_item_from(l)
 return l[ceil(rnd(#l))]
end

col_pals={
 --extract path from tile
 { 0+256,
   1+256,
   2+256,
   3+256,
   5+256,
  13+256,
  14+256,
  15+256},
 --make path grey
 { 4+16* 6,
   9+16* 6},
 --tile pre-processing
 { 0,
   7+256,
  14+16* 9
  }
}
function setpal(pal_idx)
 for v in all(
  col_pals[pal_idx]
 ) do
  if v==0 then
   palt(0,false)
  elseif v>255 then
   palt(band(v,15),true)
  else
   pal(band(v,15),shr(v,4))
  end
 end
end

function fire_event(
 listeners,source
)
 foreach(
  listeners,
  function(l) l(source) end
 )
end

--wrap coroutine with a name to
--facilitate debugging crashes
function cowrap(name,coroutine)
 local w={}
 w.name=name
 w.coroutine=cocreate(coroutine)
 return w
end

--returns true when routine died
function coinvoke(wrapped_cr)
 local cr=wrapped_cr.coroutine
 if not coresume(cr) then
  printh(
   "coroutine "..
   wrapped_cr.name.." crashed"
  )
  while true do end
 end
 return costatus(cr)=="dead"
end

function action_btnp()
 return btnp(🅾️) or btnp(❎)
end

function sleep(
 time_in_sec,can_abort
)
 for wait=1,fps*time_in_sec do
  if (
   can_abort and action_btnp()
  ) then
   return
  end
  yield()
 end
end

font={
 a={ 6,23,17,12,29,17},
 b={22,23,19,12,13, 9},
 c={ 6,21, 3,24, 0,24},
 d={22,21,19,12,21, 9},
 e={ 6,23, 3,24,24,24},
 f={22,23,17,24,24, 0},
 g={ 6,21, 3,24,28, 9},
 h={20,23,17,20,29,17},
 i={20,21,17},
 j={ 0, 0, 3,20,21, 9},
 k={20,23,17,20,13,17},
 l={20,21,19, 0, 0,24},
 m={22,21,17,14,21,17,12,21,17},
 n={22,21,17,12,21,17},
 o={ 6,21, 3,12,21, 9},
 p={22,23,17,12, 9},
 q={ 6,21, 3,12,21,25},
 r={22,23,17,12,13,17},
 s={ 6, 3,18,24,12, 9},
 t={-4, 2, 0, 0,30,21,17, 8,-4},
 u={20,21, 3,20,21, 9},
 v={20,21,19,20,21, 9},
 w={20,21, 3,20,21,11,20,21, 9},
 x={20, 7,17,20,13,17},
 y={20, 3,18,20,29, 9},
 z={18, 6,19,28, 9,24}
}
special_chars=" -()+:!,0123456789"
special_chars_spec={
 {},           -- space
 {-2,0,26,-2}, -- -
 {6,21,3,-2},  -- (
 {-2,12,21,9}, -- )
 {0,31},       -- +
 {16,0,16},    -- :
 {20,17,1},    -- !
 {-2,0,0,9},   -- ,
 {22,21,19,28,21,25}, --0
 {-2,28,21,27,-2},    --1
 {18,22,19,12,9,24},  --2
 {18,18,18,12,13,9},  --3
 {20,19,0,20,29,17},  --4
 {22,19,18,24,12,9},  --5
 {6,23,3,24,12,9},    --6
 {18,0,0,28,21,17},   --7
 {6,7,3,12,13,9},     --8
 {6,3,18,12,29,9}     --9
}
for i=1,#special_chars do
 local ch=sub(special_chars,i,i)
 font[ch]=special_chars_spec[i]
end

fontx={1,2,1,0,1}
fonty={0,1,2,1,1}
function drawtext(s,x,y)
 for i=1,#s do
  local spec=font[sub(s,i,i)]
  local row=0
  --draw character
  for j=1,#spec do
   local v=spec[j]
   if v<0 then
    x+=(v+3) --tweak spacing
   else
    if row==3 then
     row=0
     x+=3
    end
    --draw glyph
    for k=0,4 do
     if band(v,shl(1,k))!=0 then
      pset(
       x+fontx[k+1],
       y+fonty[k+1]+row*3
      )
     end
    end
    row+=1
   end
  end
  x+=2
 end
end

glyph_map={
 24, 1, 2,16, 4, 5,17,20,
  8,19, 0,23,18,22,21,15
}
function drawtitletext(s,x,y)
 palt(7,true)
 for i=1,#s do
  local spec=font[sub(s,i,i)]
  local row=0
  --draw character
  for j=1,#spec do
   local v=spec[j]
   if v>=0 then
    if row==3 then
     row=0
     x+=8
    end
    if v>0 then
     --draw glyph
     if v>=16 then
      v=glyph_map[v-15]
     end
     spr(16+v,x,y+row*8)
    end
    row+=1
   end
  end
  x+=8
 end
 pal()
end

function textwidth(s)
 local w=-(#s-1) --spacing
 for i=1,#s do
  local spec=font[sub(s,i,i)]
  local row=0
  if #spec==0 then
   w+=3
  end
  for j=1,#spec do
   local v=spec[j]
   if v<0 then
    w+=(v+3) --tweak spacing
   else
    if row==0 then
     w+=3
    end
    row=(row+1)%3
   end
  end
 end
 return w
end

digits={
 125,80,55,87,90,79,111,81,127,95
}
function draw_digit(
 digit,x0,y0,large
)
 local v=digits[digit+1]
 local d=4
 if large then
  d=9
 end
 for i=0,2 do
  if band(v,shl(1,i))!=0 then
   if large then
    spr(61,x0+2,y0+d*i)
   else
    line(
     x0+1,y0+d*i,x0+3,y0+d*i
    )
   end
  end
 end
 for j=0,3 do
  if band(v,shl(1,3+j))!=0 then
   local x=x0+d*(j%2)
   local y=y0+d*flr(j/2)+1
   if large then
    spr(45,x,y+1)
   else
    line(x,y,x,y+2)
   end
  end
 end
end

function draw_number(
 v,x,y,num_digits,large
)
 local d=7
 if large then
  d=14
 end
 for i=1,num_digits do
  local digit=v%10
  v=(v-digit)/10
  draw_digit(
   digit,x+(num_digits-i)*d,y,
   large
  )
 end
end

--len,unit,color1,color2
timebar={
 {4,1,10,9},
 {8,2,9,4},
 {16,4,4,2},
 {28,16,2,2}
}

function draw_timebar(time_left)
 if time_left<0 then
  spr(12,95,9,4,1) --timed out
  return
 end

 local i=1
 local x=126
 while (time_left>0) do
  local tb=timebar[i]
  local l=min(
   tb[1],time_left/tb[2]
  )-1
  rectfill(x-l,10,x,12,tb[3])
  line(x-l,13,x,13,tb[4])
  time_left-=tb[2]*tb[1]
  x-=tb[1]
  i+=1
 end
end

function roundrect(x1,y1,x2,y2)
 rectfill(x1+1,y1,x2-1,y2)
 line(x1,y1+1,x1,y2-1)
 line(x2,y1+1,x2,y2-1)
end

music_enabled=true

function toggle_music()
 music_enabled=not music_enabled
 if music_enabled then
  music(current_track)
 else
  music(-1)
 end
end

function switch_music(track)
 if track!=current_track then
  if music_enabled then
   music(track)
  end
  current_track=track
 end
end

function new_hiscore_mgr()
 local me={}
 local vmajor=1
 local vminor=0

 cartdata("eriban_bbots_repair")
 if dget(0)!=vmajor then
  dset(0,vmajor)
  dset(1,vminor)
  for i=2,12 do
   dset(i,0)
  end
 end

 me.level_score=function(level)
  return dget(2+level)
 end

 me.level_done=function(
  level,level_score
 )
  local old=dget(2+level)
  if level_score>old then
   dset(2+level,level_score)
  end
 end

 me.hi_score=function()
  return dget(2)
 end

 me.game_done=function(score)
  local old=dget(2)
  if score>old then
   dset(2,score)
  end
 end

 return me
end

hiscore_mgr=new_hiscore_mgr()
-->8
-- tiles
gridtile={}

_nxt_tile_idx=0
function gridtile:new(
 entries,connections,prob,
 hard_turns
)
 local o=setmetatable({},self)
 self.__index=self

 o.entries=entries
 o.connections=connections
 o.prob=prob
 o.hard_turns=hard_turns

 o.idx=_nxt_tile_idx
 _nxt_tile_idx+=1

 return o
end

function gridtile:all_entries()
 local l={}
 for d in all(dirs) do
  local bit=shl(1,d-1)
  if band(self.entries,bit)!=0 then
   add(l,d)
  end
 end
 return l
end

-- entry is dir enum
function gridtile:has_entry(
 entry
)
 local bit=shl(1,entry-1)
 return band(
  self.entries,bit
 )!=0
end

function gridtile:exits_from(
 entry
)
 assert(self:has_entry(entry))
 local l={}
 local cons=self.connections[
  entry
 ]
 for d in all(dirs) do
  local bit=shl(1,d-1)
  if band(cons,bit)!=0 then
   add(l,d)
  end
 end
 return l
end

function gridtile:dump()
 printh("idx="..self.idx)
 printh("entries="..self.entries)
 printh("exits=")
 for e in all(self.connections) do
  printh("  "..e)
 end
end

function rnd_tile_from(tiles)
 if #tiles==0 then
  return nil
 end

 local sum=0
 for tile in all(tiles) do
  sum+=tile.prob
 end
 local val=rnd(sum)
 sum=0
 for tile in all(tiles) do
  sum+=tile.prob
  if sum>=val then
   return tile
  end
 end
 assert(false)
end

no_tile=gridtile:new()
tiles={
 gridtile:new( 0,{ 0, 0, 0, 0},0),
 gridtile:new( 1,{ 1, 0, 0, 0},1),
 gridtile:new( 2,{ 0, 2, 0, 0},1),
 gridtile:new( 3,{ 2, 1, 0, 0},2),
 gridtile:new( 4,{ 0, 0, 4, 0},1),
 gridtile:new( 5,{ 4, 0, 1, 0},4),
 gridtile:new( 6,{ 0, 4, 2, 0},2),
 gridtile:new( 7,{ 2, 5, 2, 0},2),
 gridtile:new( 8,{ 0, 0, 0, 8},1),
 gridtile:new( 9,{ 8, 0, 0, 1},2),
 gridtile:new(10,{ 0, 8, 0, 2},4),
 gridtile:new(11,{10, 1, 0, 1},2),
 gridtile:new(12,{ 0, 0, 8, 4},2),
 gridtile:new(13,{ 8, 0, 8, 5},2),
 gridtile:new(14,{ 0, 4,10, 4},2),
 gridtile:new(15,{ 4, 8, 1, 2},2),
 gridtile:new( 3,{ 2, 1, 0, 0},0,true),
 gridtile:new( 6,{ 0, 4, 2, 0},0,true),
 gridtile:new(12,{ 0, 0, 8, 4},0,true),
 gridtile:new( 9,{ 8, 0, 0, 1},0,true),
 gridtile:new( 7,{ 4, 5, 1, 0},0,true),
 gridtile:new(14,{ 0, 8,10, 2},0,true),
 gridtile:new(13,{ 4, 0, 1, 5},0,true),
 gridtile:new(11,{10, 8, 0, 2},0,true)
}

function draw_tile(
 tile,screen_pos
)
 local si
 if grid.tilesize==13 then
  if tile==no_tile then
   si=46
  else
   local v=tile.idx-1
   si=64+v*2+16*flr(v/8)
  end
  spr(
   si,
   screen_pos.x,
   screen_pos.y,
   2,2
  )
 else
  if tile.idx>0 then
   spr(
    15+tile.idx,
    screen_pos.x,screen_pos.y
   )
  end
 end
end

screentile={}

function screentile:new(
 tile,screen_pos,target_pos
)
 local o=setmetatable({},self)
 self.__index=self

 o.tile=tile
 o.pos=screen_pos
 o.target_pos=target_pos

 return o
end

function screentile:update()
 self.pos:lerp(
  self.target_pos,0.1
 )
end

function screentile:draw()
 draw_tile(self.tile,self.pos)
end
-->8
--grid
tilegrid={}

function tilegrid:new(
 tilesize,width,height,mapx,mapy
)
 local o=setmetatable({},self)
 self.__index=self

 o.w=width
 o.h=height

 o.tilesize=tilesize
 tilegrid._init_origin(o)

 o.positions={}
 o.tiles={}
 for x=0,o.w-1 do
  for y=0,o.h-1 do
   local pos=vector:new(x,y)
   add(o.positions,pos)

   local t=no_tile
   if (
    mapx!=nil and mapy!=nil
   ) then
    local m=mget(mapx+x,mapy+y)
    if m>0 then
     t=tiles[m-15]
    end
   end
   local scr_t=screentile:new(
    t,vector:new(64,64)
   )
   tilegrid.place_tile(
    o,scr_t,pos,true
   )
  end
 end

 return o
end

function tilegrid:_init_origin()
 self.x0=64-flr(
  self.w*self.tilesize/2
 )+0.5
 self.y0=64-flr(
  self.h*self.tilesize/2
 )+0.5

 if self.tilesize==13 then
  --shift down to account for
  --tray
  self.y0+=7
 else
  self.y0-=2
 end
end

function tilegrid:_pos2idx(pos)
 return pos.x+pos.y*16
end

function tilegrid:_idx2pos(idx)
 return vector:new(
  idx%16,flr(idx/16)
 )
end

function tilegrid:expand(w,h)
 local w_old=self.w
 local h_old=self.h
 self.w=w
 self.h=h
 self:_init_origin()

 for x=0,w-1 do
  for y=0,h-1 do
   local pos=vector:new(x,y)
   local scr_tile
   if x>=w_old or y>=h_old then
    add(self.positions,pos)
    scr_tile=screentile:new(
     no_tile,vector:new(64,64)
    )
    self:place_tile(
     scr_tile,pos,true
    )
   else
    --adjust positions
    scr_tile=self:screentile_at(
     pos
    )
    scr_tile.target_pos=
     self:screen_pos(pos)
   end
  end
 end
end

function tilegrid:screentile_at(
 pos
)
 if self:contains(pos) then
  return self.tiles[
   self:_pos2idx(pos)
  ]
 end
 return nil
end

function tilegrid:dump_claimed()
 for p in all(self.positions) do
  local t=self.tiles[
   self:_pos2idx(p)
  ]
  if t.bot!=nil then
   printh("  claimed: "..p:to_string())
  end
 end
end

function tilegrid:num_claimed()
 local total=0
 for p in all(self.positions) do
  local t=self.tiles[
   self:_pos2idx(p)
  ]
  if t.bot!=nil then
   total+=1
  end
 end
 return total
end

function tilegrid:release_tile(
 pos,bot
)
 local t=self.tiles[
  self:_pos2idx(pos)
 ]
 if t.bot==bot then
  printh("releasing "..pos:to_string())
  t.bot=nil
  --self:dump_claimed()
 end
end

function tilegrid:claim_tile(
 pos,bot
)
 local t=self.tiles[
  self:_pos2idx(pos)
 ]
 if t.bot==nil then
  t.bot=bot
  printh("claimed "..pos:to_string())
  --self:dump_claimed()
 end

 return t.bot
end

function tilegrid:contains(pos)
 return (
  pos.x>=0 and pos.x<self.w and
  pos.y>=0 and pos.y<self.h
 )
end

function tilegrid:has_neighbour(
 pos
)
 for dr in all(dirs) do
  local np=pos+vdirs[dr]
  if self:tile_at(np)!=nil then
   return true
  end
 end
 return false
end

--ensure the tile connects to
--its neighbours (again)
function tilegrid:patch_tile(
 pos
)
 local mask=0
 for dr in all(dirs) do
  local np=pos+vdirs[dr]
  local nt=self:screentile_at(
   pos+vdirs[dr]
  )
  if (
   nt!=nil and
   nt.tile:has_entry(
    opposite(dr)
   )
  ) then
   mask+=shl(1,dr-1)
  end
 end
 local tile
 if mask==0 then
  tile=no_tile
 else
  tile=tiles[mask+1]
 end
 self:screentile_at(
  pos
 ).tile=tile
 return tile
end

function tilegrid:tile_at(pos)
 if grid:contains(pos) then
  local tile=self.tiles[
   self:_pos2idx(pos)
  ].tile
  if tile!=no_tile then
   return tile
  else
   --no tile is only used for
   --drawing purposes. pretend
   --it does not exist
   return nil
  end
 else
  --return the empty tile. this
  --prevents connections off the
  --grid
  return tiles[1]
 end
end

function tilegrid:screen_pos(pos)
 return vector:new(
  pos.x*self.tilesize+self.x0,
  pos.y*self.tilesize+self.y0
 )
end

function tilegrid:can_place_tile(
 tile,pos
)
 if tile==nil then
  return false
 end

 if self:tile_at(pos)!=nil then
  return false
 end

 local connects=false
 for dr in all(dirs) do
  local ntile=grid:tile_at(
   pos+vdirs[dr]
  )
  local haspath=tile:has_entry(
   dr
  )
  if (
   ntile!=nil and
   haspath!=ntile:has_entry(
    opposite(dr)
   )
  ) then
   return false
  end
  if haspath and ntile!=nil then
   connects=true
  end
 end

 return connects
end

function tilegrid:is_placeable(
 tile
)
 for p in all(self.positions) do
  if self:can_place_tile(
   tile,p
  ) then
   return true
  end
 end
 return false
end

function tilegrid:place_tile(
 screen_tile,pos,force
)
 assert(
  force or
  self:can_place_tile(
   screen_tile.tile,pos
  )
 )

 screen_tile.target_pos=
  self:screen_pos(pos)
 if screen_tile.pos==nil then
  screen_tile.pos=vector:new(
   screen_tile.target_pos.x,
   screen_tile.target_pos.y
  )
 end

 local i=self:_pos2idx(pos)
 self.last_removed=self.tiles[i]
 self.tiles[i]=screen_tile
 --carry over claim
 if self.last_removed!=nil then
  screen_tile.bot=
   self.last_removed.bot
 end
end

function tilegrid:update()
 for k,v in pairs(self.tiles) do
  v:update()
 end
end

function tilegrid:draw()
 setpal(3)

 if self.last_removed!=nil then
  --draw this to prevent an ugly
  --gap after placing a new tile
  self.last_removed:draw()
 end

 for k,v in pairs(self.tiles) do
  v:draw()
 end

 pal()
end
-->8
-- bot
bot={}

function move_straight(
 bot,c1,c2,c3
)
 local dr=vdirs[bot.dir]
 c1=c1 or 12
 c2=c2 or 9
 c3=c3 or 4

 return function()
  for i=1,c1 do
   if i==c2 then
    while bot:_is_blocked() do
     yield()
    end
   end

   bot.dirv:add(dr)

   if i==c3 then
    bot:_release_prv()
   end

   yield()
  end
 end
end

function tiny_move_straight(bot)
 return move_straight(
  bot,7,2,4
 )
end

function move_reverse(
 bot,c1,c2,c3,c4
)
 local dr=vdirs[bot.dir]
 c1=c1 or 6
 c2=c2 or 4
 c3=c3 or 6
 c4=c4 or 3

 return function()
  --move halfway
  for i=1,c1 do
   bot.dirv:add(dr)
   if i==c2 then
    bot:_release_prv()
   end
   yield()
  end

  --turn
  for i=1,8 do
   bot:_delta_rot(1)
   yield()
  end

  --move back
  for i=1,c3 do
   if i==c4 then
    while bot:_is_blocked() do
     yield()
    end
   end
   bot.dirv:sub(dr)
   yield()
  end
 end
end

function tiny_move_reverse(bot)
 return move_reverse(
  bot,4,2,3,2
 )
end

function move_turn(
 bot,is_hard,c1,c2
)
 local dr1=vdirs[bot.dir]
 local dr2=vdirs[bot.nxt_dir]
 c1=c1 or 3
 c2=c2 or 3

 return function()
  local o=dr2:orientation(dr1)

  --initial straight bit
  for i=1,c1 do
   bot.dirv:add(dr1)
   yield()
  end

  --move diagonally and turn
  for i=1,3 do
   if i==2 then
    bot:_release_prv()
    while bot:_is_blocked() do
     yield()
    end
   end

   bot:_delta_rot(o)
   if not is_hard then
    bot.dirv:add(dr1)
    bot.dirv:add(dr2)
   end
   yield()
  end

  --final straight bit
  bot:_delta_rot(o)
  for i=1,c2 do
   bot.dirv:add(dr2)
   yield()
  end
 end
end

function tiny_move_turn(bot)
 return move_turn(bot,false,1,0)
end

function tiny_hard_turn(bot)
 return move_turn(bot,true,4,3)
end

function flicker_rearlight(
 bot,nsteps
)
 local v=0
 for i=1,nsteps do
  v+=rnd(1)
  if flr(v)%3==0 then
   bot.rearlight_color=9
  else
   bot.rearlight_color=8
  end
  yield()
 end
end

function crash_anim(bot)
 local dr=vdirs[bot.nxt_dir]

 return function()
  --falling forward
  for i=1,4 do
   bot.crash_sprite+=2
   bot.dirv:add(dr)
   if i==3 then
    bot:_release_prv()
   end
   yield()
  end

  --actual crash
  bot.crash_sprite+=2
  bot.dirv:add(dr)
  sfx(3)
  yield()

  for i=1,2 do
   yield()
  end
  fire_event(bot.on_crash,bot)

  flicker_rearlight(bot,99)
 end
end

function pair_anim(bot,bot2)
 --how much bot should rotate in
 --order to face the other bot
 local delta=function(r)
  return abs(
   8-(16+bot2.rot-r)%16
  )
 end

 return function()
  --rotate so that bots face
  while delta(bot.rot)>0 do
   if (
    delta(bot.rot+1)<
    delta(bot.rot-1)
   ) then
    bot:_delta_rot(1)
   else
    bot:_delta_rot(-1)
   end
   yield()
  end

  --synchronize wiggle and pick
  --who initiates sound effects
  bot.done_at=time()
  local first=bot2.done_at==nil
  if (
   first or
   bot2.done_at==bot.done_at
  ) then
   yield()
  end

  bot.period=10 --fix speed

  --wiggle
  for i=1,5 do
   bot:_delta_rot(1)
   if first then sfx(13)  end
   yield()
   bot:_delta_rot(-1)
   yield()
   bot:_delta_rot(-1)
   if first then sfx(14)  end
   yield()
   bot:_delta_rot(1)
   yield()
  end

  bot:destroy()
 end
end

function clash_anim(bot)
 return function()
  bot.period=6 --fix speed
  flicker_rearlight(bot,10)
  fire_event(
   bot.on_clash,bot
  )
  flicker_rearlight(bot,99)
 end
end

bot_cr_factory_funs={
 move_straight=move_straight,
 move_turn=move_turn,
 move_reverse=move_reverse,
 crash_anim=crash_anim,
 pair_anim=pair_anim
}

tiny_bot_cr_factory_funs={
 move_straight=tiny_move_straight,
 move_turn=tiny_move_turn,
 move_hard_turn=tiny_hard_turn,
 move_reverse=tiny_move_reverse
}

bot_types={
 {30,12}, --normal blue
 {60,14}, --slow pink
 {60,12}, --slow blue
}

--pos=position
--dr0=initial direction
--typ=index into bot_types array
function bot:new(
 pos,dr0,typ,o
)
 o=setmetatable(o or {},self)
 self.__index=self

 --defaults for game-sized bot
 o.cr_funs=(
  o.cr_funs or
  bot_cr_factory_funs
 )
 o.sprite_size=(
  o.sprite_size or 2
 )
 o.sprite_index0=(
  o.sprite_index0 or 160
 )
 o.max_dirv=o.max_dirv or 6
 o.meet_dist=o.meet_dist or 11

 --type (inc. speed and color)
 o.typ=typ or 1
 local spec=bot_types[o.typ]
 o.period=o.period or spec[1]
 o.color=o.color or spec[2]

 o.rearlight_color=8
 o.clk=0

 --coarse grid movement
 local tile=grid:tile_at(pos)
 o.nxt_pos=pos
 o.nxt_dir=dr0 or opposite(
  rnd_item_from(
   tile:all_entries()
  )
 )
 assert(
  grid:claim_tile(pos,o)==o
 )
 bot._move_step(o)

 o.on_move={}
 o.on_crash={}
 o.on_clash={}

 return o
end

function bot:destroy()
 self:_release_prv()
 grid:release_tile(
  self.pos,self
 )
 grid:release_tile(
  self.nxt_pos,self
 )
 self.destroyed=true
end

function bot:_handle_crash()
 self.crashing=true
 self.crash_sprite=224-32*(
  self.nxt_dir%2
 )
 if self.nxt_dir==4 then
  self.flipx=true
  self.dirv.x-=1
 elseif self.nxt_dir==3 then
  self.flipy=true
  self.dirv.y-=1
 end
 self.period=6 --fixed speed
 sfx(5) --falling

 self.anim_cr=cowrap(
  "crash_anim",
  crash_anim(self)
 )
 self:stop()
end

function bot:_set_move_anim()
 if self.dir==self.nxt_dir then
  self.move_cr=cowrap(
   "move_straight",
   self.cr_funs.move_straight(
    self
   )
  )
 elseif abs(
  self.dir-self.nxt_dir
 )==2 then
  self.move_cr=cowrap(
   "move_reverse",
   self.cr_funs.move_reverse(
    self
   )
  )
 else
  local t=grid:tile_at(self.pos)
  if t.hard_turns then
   self.move_cr=cowrap(
    "move_turn",
    self.cr_funs.move_hard_turn(self)
   )
  else
   self.move_cr=cowrap(
    "move_turn",
    self.cr_funs.move_turn(self)
   )
  end
 end
end

function bot:_handle_move(
 tile
)
 --select next destination
 self.nxt_dir=rnd_item_from(
  tile:exits_from(
   opposite(self.dir)
  )
 )
 self.nxt_pos=
  self.pos+vdirs[self.nxt_dir]
 self:_set_move_anim()

 fire_event(self.on_move,self)
end

function bot:_move_step()
 self.prv_pos=self.pos
 self.pos=self.nxt_pos
 self.dir=self.nxt_dir

 --fine-grained drawing state
 local entry=opposite(self.dir)
 local dirv=vdirs[entry]
 self.dirv=dirv*self.max_dirv
 self.rot=(self.dir-1)*4

 local t=grid:tile_at(self.pos)
 if t!=nil then
  self:_handle_move(t)
 else
  self:_handle_crash()
 end
end

--release previous tile
function bot:_release_prv()
 printh("releasing previous")
 if self.prv_pos!=nil then
  grid:release_tile(
   self.prv_pos,self
  )
  self.prv_pos=nil
 end
end

--is the nxt nxt position
--already known?
function bot:_forced_nxt2_pos()
 local t=grid:tile_at(
  self.nxt_pos
 )
 if t==nil then
  return nil
 end
 local exits=t:exits_from(
  opposite(self.nxt_dir)
 )
 if #exits!=1 then
  return nil
 end

 --return the forced nxt dest
 return (
  self.nxt_pos+vdirs[exits[1]]
 )
end

function bot:_will_meet_with(
 bot2
)
 if (
  bot2.nxt_pos==self.pos
 ) then
  --the bots will meet
  return true
 end

 if (
  bot2.nxt_pos!=self.nxt_pos
 ) then
  --the bots will not meet.
  --the other bot may be leaving
  --the claimed tile to another
  --tile
  return false
 end

 --they will visit the same tile
 --but may still avoid collision
 return (
  bot2:_forced_nxt2_pos()==
   self.pos and
  self:_forced_nxt2_pos()==
   bot2.pos
 )
end

--check if next tile is free. if
--not, claim it
function bot:_is_blocked()
 printh("check blocked")
 if self.meeting!=nil then
  --never blocked when meeting
  return false
 end
 local claimer=grid:claim_tile(
  self.nxt_pos,self
 )
 if claimer==self then
  --managed to claim the tile
  return false
 end

 if (
  self:_will_meet_with(claimer)
 ) then
  self.meeting=claimer
  claimer.meeting=self
  return false
 end

 --tile is blocked by another
 --bot but our paths will not
 --cross, so just wait
 printh("tile "..self.nxt_pos:to_string()..
  " is blocked")
 return true
end

function bot:_delta_rot(d)
 self.rot=(self.rot+16+d)%16
end

function bot:_paired()
 self.anim_cr=cowrap(
  "pair_anim",
  pair_anim(self,self.meeting)
 )
 self.meeting=nil
 self:stop()
end

function bot:_clashed()
 self.anim_cr=cowrap(
  "clash_anim",
  clash_anim(self)
 )
 self.meeting=nil
 self:stop()
end

function bot:stop()
 printh("bot stopped")
 self.move_cr=nil
end

function bot:_handle_meeting()
 local mbot=self.meeting
 local p1=grid:screen_pos(
  self.pos
 )+self.dirv
 local p2=grid:screen_pos(
  mbot.pos
 )+mbot.dirv
 printh("dist="..p1:dist(p2))
 if (
  p1:dist(p2)<=self.meet_dist
 ) then
  if self.typ==mbot.typ then
   self.meeting:_paired()
   self:_paired()
  else
   sfx(12)
   self.meeting:_clashed()
   self:_clashed()
  end
 end
end

function bot:update()
 if self.destroyed then
  return false
 end

 self.clk=(self.clk+1)%self.period
 if self.clk>0 then
  return true
 end

 if (
  self.anim_cr!=nil and
  coinvoke(self.anim_cr)
 ) then
  self.anim_cr=nil
 end

 if self.move_cr!=nil then
  if coinvoke(self.move_cr) then
   self:_move_step()
  end

  if self.meeting!=nil then
   self:_handle_meeting()
  end
 end

 return true
end

function bot:draw()
 local pos=grid:screentile_at(
  self.pos
 ).pos

 pal(12,self.color) --main color
 pal(8,self.rearlight_color)

 if self.crashing then
  spr(
   self.crash_sprite,
   pos.x+self.dirv.x-1,
   pos.y+self.dirv.y-1,
   self.sprite_size,
   self.sprite_size,
   self.flipx,self.flipy
  )
 else
  local si=self.rot%8
  if self.rot>7 then
   --invert rear/front lights
   pal(8,10)
   pal(10,self.rearlight_color)
  end
  spr(
   self.sprite_index0
   +si*self.sprite_size,
   pos.x+self.dirv.x,
   pos.y+self.dirv.y,
   self.sprite_size,
   self.sprite_size
  )
 end
 pal()
end

function bot:dump()
 printh("pos="..self.pos:to_string())
 printh("dir="..self.dir)
 printh("nxt_pos="..self.nxt_pos:to_string())
 printh("nxt_dir="..self.nxt_dir)
end

-->8
-- tiletray

tiletray={}

function tiletray:new(size)
 local o=setmetatable({},self)
 self.__index=self

 o.size=size

 o.xsep=2
 local w=(
  size*grid.tilesize+
  (size-1)*o.xsep
 )
 o.x0=63-flr(w/2)
 o.y0=1.5

 o.cursor_pos=vector:new(
  o.x0,o.y0
 )

 o.tiles={}
 o.num_tiles=0
 tiletray._replenish(o)

 o.on_placed={}
 o.on_complete={}

 return o
end

function tiletray:_has_tile(tile)
 for t in all(self.tiles) do
  if t.tile==tile then
   return true
  end
 end
 return false
end

function tiletray:_new_tile()
 local l={}
 for t in all(tiles) do
  if (
   t.prob>0 and
   not self:_has_tile(t) and
   grid:is_placeable(t)
  ) then
   add(l,t)
  end
 end
 return rnd_tile_from(l)
end

function tiletray:_update_target_pos()
 local mul=(
  grid.tilesize+self.xsep
 )
 for i,t in pairs(self.tiles) do
  t.target_pos=vector:new(
   self.x0+(i-1)*mul,self.y0
  )
 end
end

function tiletray:_pop()
 assert(self.num_tiles>0)

 local popped=self.tiles[1]

 for i=1,self.num_tiles-1 do
  self.tiles[i]=self.tiles[i+1]
 end

 self.tiles[self.num_tiles]=nil
 self.num_tiles-=1
 self:_update_target_pos()

 return popped
end

function tiletray:_push(tile)
 assert(self.num_tiles<self.size)

 for i=self.num_tiles,1,-1 do
  self.tiles[i+1]=self.tiles[i]
 end

 self.tiles[1]=screentile:new(
  tile,
  vector:new(
   self.x0,-grid.tilesize
  )
 )
 self.num_tiles+=1
 self:_update_target_pos()
end

function tiletray:_replenish()
 assert(self.num_tiles<self.size)

 local num0=self.num_tiles
 while (
  self.num_tiles<self.size
 ) do
  local tile=self:_new_tile()
  if tile==nil then
   --no new tile is placeable
   return num0!=self.num_tiles
  else
   self:_push(tile)
  end
 end

 return true
end

function tiletray:switch()
 assert(self.num_tiles>=2)

 local tmp=self.tiles[1]
 for i=1,self.num_tiles-1 do
  self.tiles[i]=self.tiles[i+1]
 end
 self.tiles[self.num_tiles]=tmp

 self:_update_target_pos()
 sfx(0)
end

function tiletray:_done()
 for t in all(self.tiles) do
  if grid:is_placeable(
   t.tile
  ) then
   return false
  end
 end

 --none of the tiles on the tray
 --cam be put on the grid
 return true
end

function tiletray:place_tile(
 pos
)
 grid:place_tile(
  self:_pop(),pos
 )
 sfx(2)
 fire_event(self.on_placed,self)

 if (
  not self:_replenish() and
  self:_done()
 ) then
  fire_event(
   self.on_complete,self
  )
 end
end

function tiletray:selected_tile()
 if self.num_tiles==0 then
  return nil
 else
  return self.tiles[1].tile
 end
end

function tiletray:update()
 foreach(
  self.tiles,
  screentile.update
 )
end

function tiletray:draw()
 setpal(3)
 foreach(
  self.tiles,
  screentile.draw
 )
 pal()

 if #self.tiles>0 then
  draw_cursor(self.cursor_pos)
 end
end

-->8
--cursor & lives

gridcursor={}

function gridcursor:new(pos)
 local o=setmetatable({},self)
 self.__index=self

 o.pos=pos
 o.draw_pos=vector:new(0,0)

 o.contraction=0
 o.contraction_clk=0
 gridcursor._pos_changed(o)

 return o
end

function gridcursor:_check_allowed()
 self.allowed=grid:can_place_tile(
  tray:selected_tile(),
  self.pos
 )
end

function gridcursor:_pos_changed()
 self.draw_pos_target=
  grid:screen_pos(self.pos)
 self:_check_allowed()
end

function gridcursor:update()
 local pos=self.pos
 local pos_changed=false
 if btnp(⬅️) then
  pos.x=(pos.x+grid.w-1)%grid.w
  pos_changed=true
 end
 if btnp(➡️) then
  pos.x=(pos.x+1)%grid.w
  pos_changed=true
 end
 if btnp(⬆️) then
  pos.y=(pos.y+grid.h-1)%grid.h
  pos_changed=true
 end
 if btnp(⬇️) then
  pos.y=(pos.y+1)%grid.h
  pos_changed=true
 end

 if pos_changed then
  printh("pos changed")
  self:_pos_changed()
 end

 if btnp(❎) then
  if tray.num_tiles>=2 then
   printh("tray switch")
   tray:switch()
   self:_check_allowed()
  else
   sfx(1) --no can do
  end
 end

 if btnp(🅾️) then
  if (
   self.allowed and
   not bot_at(self.pos)
  ) then
   printh("placed tile")
   tray:place_tile(self.pos)
   self.allowed=false
   self.contraction_clk=20
  else
   sfx(1) --no can do
  end
 end

 if self.contraction_clk>0 then
  self.contraction_clk-=1
  self.contraction=(
   0.5-0.5*cos(
    self.contraction_clk/20
   )
  )
 end

 self.draw_pos:lerp(
  self.draw_pos_target,0.25
 )
end

function draw_cursor(
 pos,contraction
)
 local d=0
 if contraction!=nil then
  d=flr(contraction*4+0.5)
 end
 spr(
  9,pos.x+d,pos.y+d
 )
 spr(
  9,pos.x+5-d,pos.y+d,
  1,1,true
 )
 spr(
  9,pos.x+d,pos.y+5-d,
  1,1,false,true
 )
 spr(
  9,pos.x+5-d,pos.y+5-d,
  1,1,true,true
 )
end

function gridcursor:draw()
 if self.contraction==0 then
  setpal(3)
  setpal(1) --extract path
  if not self.allowed then
   setpal(2) --grey out path
  end

  local t=tray:selected_tile()
  if t!=nil then
   draw_tile(t,self.draw_pos)
  end

  pal()
 end

 draw_cursor(
  self.draw_pos,self.contraction
 )
end

function new_lives()
 local me={}

 local num_lives=3
 local draw_lives=0
 local clk=0

 function me.inc()
  --lives can temp exceed max
  num_lives+=1
 end

 function me.dec()
  local can_dec=num_lives>0
  if can_dec then
   num_lives-=1
  end
  return can_dec
 end

 function me.update()
  clk+=1
  if clk%5==0 then
   local d=num_lives*8-draw_lives
   if d!=0 then
    draw_lives+=sgn(d)
    if draw_lives==32 then
     --do not let lives equal 4
     num_lives-=1
     score+=100
    end
   end
  end
 end

 function me.draw()
  for i=1,flr(draw_lives/8) do
   spr(48,128-8*i,1)
  end
  local m=draw_lives%8
  if m!=0 then
   spr(
    56-m,
    128-8*ceil(draw_lives/8),0
   )
  end
 end

 return me
end
-->8
--game

function new_game()
 level=1
 lives=new_lives()
 score=0
 draw_score=score
 speedup_cr=nil
 start_level()
end

function load_level(lspec)
 local gs=lspec.grid
 grid=tilegrid:new(
  13,gs[1],gs[2],gs[3],gs[4]
 )

 bots={}
 for bs in all(lspec.bots) do
  local bot=bot:new(
   vector:new(bs[1],bs[2]),
   bs[3],bs[4]
  )

  add(bots,bot)
  add(bot.on_move,on_move)
  add(bot.on_crash,on_crash)
  add(bot.on_clash,on_clash)
 end

 tray=tiletray:new(
  lspec.misc[2] or 3
 )
 add(
  tray.on_placed,on_tile_placed
 )
 add(
  tray.on_complete,on_grid_done
 )

 curs=gridcursor:new(
  vector:new(4,3)
 )

 ticks_remaining=
  fps*lspec.misc[1]

 _update60=update_game
 _draw=draw_game

 menuitem(
  1,"end attempt",on_user_abort
 )
 menuitem(
  2,"end game",on_user_end_game
 )

 switch_music(0)
end

function start_level()
 local lspec=levelspecs[level]

 local t=0
 local s=(
  "level "..level..": "..
  lspec.name
 )
 local w=textwidth(s)
 local x=64-flr(w/2)
 local p=vector:new(x,0)
 local tp=vector:new(x,50)

 switch_music(-1)
 level_start_score=score

 _draw=function()
  cls()
  color(4)
  roundrect(x-1,p.y-1,x+w,p.y+9)
  color(9)
  drawtext(s,p.x,p.y)
  if t>60 then
   color(9)
   drawtext("get ready!",41,62)
  end
 end
 _update60=function()
  p:lerp(tp,0.1)
  t+=1
  if t==60 then
   sfx(7)
  end
  if (
   t==180 or action_btnp()
  ) then
   load_level(lspec)
  end
 end
end

function update_game()
 grid:update()
 for bot in all(bots) do
  if not bot:update() then
   del(bots,bot)
   if #bots==0 then
    on_grid_cleared()
   end
  end
 end
 if curs!=nil then
  curs:update()
 end
 if tray!=nil then
  tray:update()
 end
 lives.update()

 if end_anim!=nil then
  if coinvoke(end_anim) then
   end_anim=nil
  end
 else
  ticks_remaining-=1
  if ticks_remaining<0 then
   sfx(8)
   on_death()
  end
 end

 if draw_score<score then
  draw_score+=1
 end

 if (
  speedup_cr!=nil and
  coinvoke(speedup_cr)
 ) then
  speedup_cr=nil
 end
end

function draw_game()
 cls()

 grid:draw()
 foreach(bots,bot.draw)
 if curs!=nil then
  curs:draw()
 end
 if tray!=nil then
  tray:draw()
 end
 lives.draw()

 if (
  score>=hiscore_mgr.hi_score()
 ) then
  color(10)
 else
  color(4)
 end
 draw_number(draw_score,1,3,5)

 draw_timebar(
  ticks_remaining/fps
 )
end

function bot_at(pos)
 for bot in all(bots) do
  if bot.pos==pos then
   return true
  end
 end
 return false
end

function on_tile_placed(tray)
 score+=10
end

function disable_input()
 curs=nil
 tray=nil
end

function on_grid_done(tray)
 printh("grid done!")
 disable_input()

 speedup_cr=cowrap(
  "bot_speedup",
  bot_speedup
 )
end

function on_move(bot)
 printh("bot moved to "..bot.pos:to_string())
end

function on_grid_cleared()
 printh("all bots paired")

 sfx(9)
 end_play()
 end_anim=cowrap(
  "level_done_anim",
  level_done_anim
 )
end

function on_crash(bot)
 printh("bot crashed")

 if end_anim==nil then
  --act only on first crash
  sfx(4)
  on_death()
 end
end

function on_clash(bot)
 printh("bots clashed")

 if end_anim==nil then
  sfx(4)
  on_death()
 end
end

function end_play()
 switch_music(-1)

 --remove menu items
 menuitem(1)
 menuitem(2)

 disable_input()

 for bot in all(bots) do
  bot:stop()
 end
end

function on_death()
 end_play()

 if lives.dec() then
  end_anim=cowrap(
   "retry_anim",retry_anim
  )
 else
  game_end(2)
 end
end

function on_user_abort()
 sfx(4)
 on_death()
end

function on_user_end_game()
 end_play()
 game_end()
end

--starts end game animation
function game_end(wait)
 printh("game end")

 end_anim=cowrap(
  "game_end_anim",
  function()
   sleep(wait or 0)
   sfx(10)
   game_end_anim()
  end
 )
end

function game_end_anim()
 local morph_text=12 --game over
 if level>#levelspecs then
  morph_text=21 --the end
 end
 morph_grid(morph_text)

 --score remaining lives
 while lives.dec() do
  score+=100
  while draw_score<score do
   sfx(6)
   yield()
  end
  sleep(1)
 end

 hiscore_mgr.game_done(score)

 sleep(30,true)
 mainmenu()
end

function retry_anim()
 sleep(3)
 start_level()
end

function bot_speedup()
 local done=false
 while not done do
  done=true
  for bot in all(bots) do
   if (
    bot.period>1 and
    bot.anim_cr==nil
   ) then
    bot.period=ceil(bot.period/2)
    done=false
   end
  end
  sleep(0.5)
 end
end

function level_done_anim()
 --wiggle time!
 sleep(3)

 lives.inc()
 sfx(11)

 sleep(2)

 while draw_score<score do
  sfx(6)
  yield()
 end

 sleep(0.5)

 while ticks_remaining>0 do
  ticks_remaining=max(
   0,ticks_remaining-30
  )
  score+=1
  sfx(6)
  yield()
 end

 hiscore_mgr.level_done(
  level,score-level_start_score
 )

 sleep(2,true)

 level+=1
 if level<=#levelspecs then
  start_level()
 else
  game_end_anim()
 end
end

function morph_grid(map_x0)
 local tile_status={}
 local ready={}

 local target_tile=function(pos)
  local v=mget(
   pos.x+map_x0,pos.y+16
  )
  if v!=0 then
   return tiles[v-15]
  else
   return no_tile
  end
 end

 grid:expand(9,8)
 for p in all(grid.positions) do
  local i=grid:_pos2idx(p)
  if (
   grid:screentile_at(
    p
   ).tile==target_tile(p)
  ) then
   --it's already correct
   tile_status[i]=2
  elseif (
   grid:tile_at(p)!=nil or
   grid:has_neighbour(p)
  ) then
   --it can be updated
   tile_status[i]=1
   add(ready,i)
  else
   --this tile is empty. wait
   --until it has at least one
   --neighbour
   tile_status[i]=0
  end
 end

 local place_tile=function(pos)
  local tile=target_tile(pos)
  grid:place_tile(
   screentile:new(tile),pos,true
  )
 end

 local update_tile=function(pos)
  local i=grid:_pos2idx(pos)
  if tile_status[i]==0 then
   tile_status[i]=1
   add(ready,i)
  end
  if tile_status[i]==1 then
   local tile=grid:patch_tile(
    pos
   )
   if (
    tile==target_tile(pos)
   ) then
    --no further update needed
    del(ready,i)
    tile_status[i]=2
   end
  end
 end

 local anim=function()
  sleep(1)

  while #ready>0 do
   local i=rnd_item_from(ready)
   del(ready,i)
   local p=grid:_idx2pos(i)
   place_tile(p)
   tile_status[i]=2

   for dr in all(dirs) do
    local np=p+vdirs[dr]
    if grid:contains(np) then
     update_tile(np)
    end
   end
   yield()
  end
 end

 anim()
end
-->8
-- main

levelspecs={
{
 name="warm-up",
 grid={9,7,44,0},
 bots={
  {1,0,2},{7,0,4}
 },
 misc={30}
},{
 name="no choice",
 grid={9,7,35,0},
 bots={{0,5,3,2},{8,1,1,2}},
 misc={180,1}
},{
 name="spacious",
 grid={9,7,0,0},
 bots={{0,6},{8,0}},
 misc={180}
},{
 name="detour",
 grid={9,7,9,0},
 bots={{4,0,4,2},{4,6,2,2}},
 misc={240}
},{
 name="death valley",
 grid={9,8,18,0},
 bots={{1,7,4},{7,0,2}},
 misc={240}
},{
 name="cramped",
 grid={8,8,27,0},
 bots={{0,5,4},{7,2,2}},
 misc={240}
},{
 name="four of a kind",
 grid={9,8,0,7},
 bots={{3,0,4},{8,3,1},
       {5,7,2},{0,4,3}},
 misc={240}
},{
 name="two pair",
 grid={9,8,9,7},
 bots={{3,0,4,2},{8,3,1,3},
       {5,7,2,2},{0,4,3,3}},
 misc={240}
}}

function _init()
 printh("---- init ----")
 mainmenu()
end

function mainmenu()
 switch_music(25)

 local make_bot=function(pos)
  return bot:new(
   pos,nil,nil,{
    cr_funs=tiny_bot_cr_factory_funs,
    sprite_size=1,
    sprite_index0=1,
    max_dirv=4,
    period=5
   }
  )
 end

 grid=tilegrid:new(8,12,11,0,16)
 local bots={}
 add(
  bots,
  make_bot(vector:new(0,0))
 )
 add(
  bots,
  make_bot(vector:new(11,10))
 )

 local clk=0
 local buttons_enabled=false
 local active_button=1
 local menu_actions={
  show_help,new_game,show_hof,
  toggle_music
 }

 local draw_button=function(idx)
  local s=204+idx*2+flr(idx/2)*28
  local x=22+idx*23
  local disabled=(
   idx==3 and
   not music_enabled
  )
  if idx==active_button then
   if not disabled then
    pal(9,4)
   end
   palt(0,true)
   for i=1,4 do
    local d=vdirs[i]
    spr(s,x+d.x,112+d.y,2,2)
   end
   pal()
  end
  if disabled then
   pal(9,4)
  end
  spr(s,x,112,2,2)
  pal()
 end

 _update60=function()
  clk+=1
  if clk==fps then
   buttons_enabled=true
  end

  grid:update()
  for bot in all(bots) do
   if not bot:update() then
    del(bots,bot)
   end
  end

  if buttons_enabled then
   if btnp(➡️) then
    active_button=(
     active_button+1
    )%4
   end
   if btnp(⬅️) then
    active_button=(
     active_button+3
    )%4
   end
   if btnp(🅾️) then
    menu_actions[
     active_button+1
    ]()
   end
  end
 end

 _draw=function()
  cls()

  rectfill(16,18,111,106,1)
  grid:draw()
  foreach(bots,bot.draw)
  --for cart label
  --spr(1,96,64)
  --spr(8,24,42)

  palt(7,true)
  palt(0,false)
  spr(40,17,42,1,2)
  spr(42,17,56,1,2)
  spr(40,17,69,1,2)
  spr(41,103,42,1,2)
  spr(40,103,56,1,2)
  spr(41,103,70,1,2)
  spr(60,72,59)
  spr(60,72,67)
  spr(59,64,97)
  spr(43,49,83)
  pal()

  color(4)
  roundrect(27,2,99,12)
  color(9)
  drawtext(
   "eriban presents",28,3
  )

  if buttons_enabled then
   for i=0,3 do
    draw_button(i)
   end
  end
 end
end

function update_back2menu()
 if action_btnp() then
  mainmenu()
 end
end

function show_hof()
 _update60=update_back2menu
 _draw=draw_hof
end

function show_help()
 _update60=update_back2menu
 _draw=draw_help
end

function draw_hof()
 cls()

 local drwrect=function(x,y,w,h)
  rect(x,y,x+w-1,y+h-1,5)
  rectfill(x+1,y+1,x+w-2,y+h-2,0)
 end

 drawtitletext("hi-scores",0,0)

 rectfill(5,26,122,127,1)
 drwrect(27,28,74,27)

 draw_number(
  hiscore_mgr.hi_score(),
  30,31,5,true
 )

 for i=0,9 do
  local right=flr(i/5)
  local x=right*64+10
  local y=(i%5)*14+59
  local x1=x+right*32
  local x2=x+(1-right)*20-1
  drwrect(x1-2,y-2,16,13,4)
  drwrect(x2-2,y-2,30,13,4)
  color(9)
  draw_number(i+1,x1,y,2)
  draw_number(
   hiscore_mgr.level_score(i+1),
   x2,y,4
  )
 end
end

function draw_help()
 cls()

 drawtitletext("help",32,0)

 color(9)
 roundrect(4,27,123,37)
 color(0)
 drawtext(
  "re-unite (re-pair) the bots",5,28
 )

 color(9)
 drawtext(
  "guide them by extending the",2,45
 )
 drawtext(
  "tracks they traverse",15,54
 )

 drawtext(
  "changes the selected tile",11,72
 )
 drawtext(
  "puts it on the grid when",11,84
 )
 spr(10,2,73)
 spr(11,2,85)
 drawtext(
  "it fits and extends a path",11,93
 )
end

function draw_oneline_title()
 cls()

 local dg=function(
  x0,y0,w,map_x
 )
  local map_y=27
  palt(7,true)
  for x=0,w-1 do
   for y=0,3 do
    local m=mget(
     map_x+x,map_y+y
    )
    if m>0 then
     spr(m,x0+x*7,y0+y*7)
    end
   end
  end
 end

 dg(0,25,13,0)
 dg(0,60,9,12)
 dg(0,95,13,21)
end

__gfx__
000000000000000000ca0000000000000000cc000000000000cc00000000000000008c0066600000099999009900099099909090009099909900009009090999
000000000caaaac000ccaa00000cca0000ccca000cccccc0008ccc000008cc000088cc0060000000999999909990999009009099099090009090090909090090
007007000cccccc00cccccac00cccca0cccccca008cccca008cccccc008cccc0c8ccccc060000000990009900999990009009090909099009090090909090090
000770000cccccc00ccccccc0cccccca8ccccca008cccca008ccccca08ccccccccccccc000000000990009900099900009009090009090009090090909090090
000770000cccccc0ccccccc00ccccccc08ccccca08cccca08ccccca00ccccccc0ccccccc00000000990009900999990009009090009099909900009000990090
007007000cccccc0c8ccccc008cccccc08cccccc08cccca0cccccca00cccccca0cccccac00000000999999909990999000000000000000000000000000000000
000000000c8888c00088cc00008cccc0008ccc000cccccc000ccca0000cccca000ccaa0000000000099999009900099000000000000000000000000000000000
000000000000000000008c000008cc0000cc0000000000000000cc00000cca0000ca000000000000000000000000000000000000000000000000000000000000
77777777774994777777777777499477777777777749947777777777774994777777777777499477777777777749947777777777774994777777777777499477
77777777774994777777777777499477777777777749947777777777774994777777777777499477777777777749947777777777774994777777777777499477
77444477774994777774444477499944777447777749947777777444774999444444477744999477444444444499994444477777449994774447744444499444
74999947774994777749999977749999774994777749947777774999777499999999947799994777999999999999999999947777999947779994499999999999
74999947774994777749999977774999774994777749947777749999777499999999947799947777999999999994499999994777999947779999999999999999
77444477777447777774444477777444774994777749947777499944774999444444477744477777444444444447744444999477449994774499994444499444
77777777777777777777777777777777774994777749947777499477774994777777777777777777777777777777777777499477774994777749947777499477
77777777777777777777777777777777774994777749947777499477774994777777777777777777777777777777777777499477774994777749947777499477
77499477777777777777777777499477774994777777777777499477774994777777777777777777777777777dddd77777ddd777090000001111111111111777
7749947777777777777777777749947777499477777777777749947777499477777797777777977777779777dd55dd777d555d779a9000001111111111111777
7749944477444444444444774449947777499444444444444449947744499444777797777777977777779777d5555d77d55555d79a9000001111111111111777
7749999977499999999994779999947777499999999999999999947799999999777555777799777777755577d5555d77d55555d79a9000001111111111111777
7749999977499999999994779999947777499999999999999999947799999999777ddd7777779977777ddd77dd55dd77d55555d79a9000001111111111111777
77444444774994444449947744444477774994444449944444499477444444447772227777997777777555777dddd7777d555d779a9000001111111111111777
7777777777499477774994777777777777499477774994777749947777777777777ddd7777779977777ddd777777777777ddd7779a9000001111119111111777
77777777774994777749947777777777774994777749947777499477777777777771117777997777777eee777777777777777777090000001111111111111777
0440440004404400000000000000000000000000000000000000000000000000777ddd7777779977777ddd777dd7777777000777099999901111111111111777
4444444004444400044044000440440000404000000000000000000000000000777555777799777777755577d55d7777790009779aaaaaa91111111111111777
4444444004444400044444000444440000444000004040000040400000000000777797777777977777779777d55d777777000777099999901111111111111777
44444440044444000444440000444000004440000044400000040000000400007777977777779777777797777dd77dd779000977000000001111111111111777
04444400004440000044400000040000000400000004000000000000000000007777777777777777777777777777d55d77000777000000001111111111111777
00444000000400000004000000000000000000000000000000000000000000007777777777777777777777777777d55d79000977000000007777777777777777
000400000000000000000000000000000000000000000000000000000000000077777777777777777777777777777dd777000777000000007777777777777777
00000000000000000000000000000000000000000000000000000000000000007777777777777777777777777777777777777777000000007777777777777777
11111111111117771111149411111777111111111111177711111494111117771111111111111777111114941111177711111111111117771111149411111777
11111111111117771111149411111777111111111111177711111494111117771111111111111777111114941111177711000111111117771111149411111777
111111111111177711111494111117771111111111111777111114941111177711111111111117771111149411e117771e000e111111177711e1149411111777
111111111111177711111494111117771111111111111777111114941111177711111111111117771111149411e11777110001111111177711e1149411111777
1111144411111777111114941111177711111444111117771111114941111777111114441111177711111494155517771e000e11111117771555114941111777
11114999411117771111499941111777111149994444477711111114944447771111499941111777111114941ddd177711000111144447771111111494444777
111149994111177711114999411117771111499999999777111ddd1149999777111149994111177711111494122217771e000e11499997771222111149999777
11114999411117771111499941111777111149994444477711d555d1144447771111499941111777111114941fff177711000114944447771fff111494444777
11111444111117771111144411111777111114441111177711d555d1111117771111149411111777111114941555177711111149411117771555114941111777
11111111111117771111111111111777111111111111177711d555d11111177711111494111117771111149411e11777111114941111177711e1149411111777
111111111111177711111111111117771111111111111777111ddd111111177711111494111117771111149411e11777111114941111177711e1149411111777
11111111111117771111111111111777111111111111177711111111111117771111149411111777111114941111177711111494111117771111149411111777
11111111111117771111111111111777111111111111177711111111111117771111149411111777111114941111177711111494111117771111149411111777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
11111111111117771111149411111777111111111111177711111494111117771111111111111777111114941111177711111111111117771111149411111777
1111111111111777111114941111177711111e1e1111177711111494111117771111512f51111777111114941111177711111111111117771111149411111777
1111111111111777111114941111177711ee1e1e1ee11777111114941111177711ee512f5ee117771111149411e1177711111111111117771111149411111777
111111111111177711111494111117771111e1e1e111177711111494111117771111512f511117771111149411e1177711111111111117771111149411111777
1111144411111777111149411dd11777111111111111177711114949411117771111111111111777111149411e11177711111111111117771111149411111777
444449994111177744449411d55d17774444444444444777444494149444477744441111111117774444941111ee177744441111144447774444449444444777
999999994111177799994111d55d1777999999999999977799994111499997779999411111111777999941111e11177799994111499997779999999999999777
4444499941111777444411111dd117774444444444444777444411111444477744449411111117774444941111ee177744449414944447774444449444444777
111114441111177711111dd111111777111111111111177711111111111117771111494111111777111149411e11177711114949411117771111149411111777
11111111111117771111d55d111117771111111111111777111111111111177711111494111117771111149411e1177711111494111117771111149411111777
11111111111117771111d55d111117771111111111111777111111111111177711111494111117771111149411e1177711111494111117771111149411111777
111111111111177711111dd111111777111111111111177711111111111117771111149411111777111114941111177711111494111117771111149411111777
11111111111117771111111111111777111111111111177711111111111117771111149411111777111114941111177711111494111117771111149411111777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
11111494111117771111111111111777111111111111177711111494111117771111149411111777111111111111177711111494111117771111149411111777
1111149411111777111111111111177711111e1e111117771111149411111777111114941111177711115f215111177711111494111117771111149411111777
111114941111177711e111111111177711ee1e1e1ee11777111114941111177711e114941111177711ee5f215ee1177711111494111117771111149411111777
111114941111177711e11111111117771111e1e1e1111777111114941111177711e114941111177711115f215111177711111494111117771111149411111777
11111494111117771555111111111777111111111111177711111494111117771555149411111777111111111111177711111494111117771111149411111777
11111494444447771fff144444444777444444441111177744444494111117771111149444444777444444444444477744444494111117774444449444444777
11111499999997771ddd149999999777999999941111177799999994111117771fff149999999777999999999999977799999994111117779999999999999777
11111444444447771222149444444777444444941111177744444444111117771ddd149444444777444444944444477744444494111117774444444444444777
111111111111177715551494111117771111149411111777111111111dd117771555149411111777111114941111177711111494111117771111111111111777
111152f25111177711e1149411111777111114941111177711111111d55d177711e1149411111777111114941111177711111494111117771111e1e1e1111777
11ee52f25ee1177711e1149411111777111114941111177711111111d55d177711e11494111117771111149411111777111114941111177711ee1e1e1ee11777
111152f25111177711111494111117771111149411111777111111111dd1177711111494111117771111149411111777111114941111177711111e1e11111777
11111111111117771111149411111777111114941111177711111111111117771111149411111777111114941111177711111494111117771111111111111777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
00000000000000000000000000000000000000a00000000000000000000000000000000000000000000000000000000000000080000000000000000000000000
00000000000000000000aa000000000000000aca000000000000000aa000000000000000000000000000088000000000000008c8000000000000000880000000
00aaaaaaaaa000000000acaa0000000000000ccca0000000000000cca0000000008800000aa00000000008cc0000000000008ccc000000000000088c80000000
00accccccca000000000ccccaa0000000000ccccca0000000000ccccca000000008ccccccca0000000008ccccc0000000008ccccc000000000088cccc0000000
000ccccccc000000000cccccccaa0000000ccccccca00000088cccccca000000008ccccccca0000000008ccccccaa000008ccccccc000000088ccccccc000000
000ccccccc000000000cccccccca000008ccccccccca000008cccccccca00000008ccccccca000000008cccccccca00008ccccccccca000008cccccccc000000
000ccccccc00000000ccccccccc000008ccccccccccca000008ccccccca00000008ccccccca000000008ccccccca00008ccccccccccca00000ccccccccc00000
000ccccccc00000008cccccccc00000008ccccccccca0000008cccccccca0000008ccccccca00000008cccccccca000008ccccccccca0000000cccccccca0000
000ccccccc000000088ccccccc000000008ccccccc0000000008ccccccaa0000008ccccccca000000088cccccca00000000ccccccca00000000cccccccaa0000
008ccccccc80000000088cccc00000000008ccccc00000000008ccccc0000000008ccccccca0000000000ccccca000000000ccccca0000000000ccccaa000000
00888888888000000000088c8000000000008ccc0000000000008cc000000000008800000aa000000000000cca00000000000ccca00000000000acaa00000000
00000000000000000000000880000000000008c8000000000000880000000000000000000000000000000000aa00000000000aca000000000000aa0000000000
000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000a0000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999990000000000090000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009999999000000000099000000000
000accccccca00000000ccccccc000000000ccccccc000000000ccccccc000000000ccccccc00000000000000000000000099999999900000000099900000000
0000ccccccc00000000accccccca00000000ccccccc000000000ccccccc000000000ccccccc000000000ddddddd0000000099900099900000000099990000000
0000ccccccc000000000ccccccc00000000accccccca0000000accccccca00000000ddddddd000000000ddddddd0000000099900099900000000099999000000
0000ccccccc000000000ccccccc000000000ccccccc000000000ddddddd00000000addddddda00000000ddddddd0000000099900099900000000099999900000
0000ccccccc000000000ccccccc000000000ccccccc000000000ddddddd000000000ddddddd00000000888888888000000000000999900000000099999990000
0000ccccccc000000000ccccccc000000000ddddddd000000000ddddddd0000000088888888800000000ddddddd0000000000009999000000000099999999000
0000ccccccc000000000ddddddd000000000ddddddd0000000088888888800000000ddddddd000000000ddddddd0000000000099990000000000099999990000
0000ddddddd00000000888888888000000088888888800000000ddddddd000000000ddddddd000000000ddddddd0000000000099900000000000099999900000
00088888888800000000ddddddd000000000ddddddd000000000ddddddd000000000ddddddd00000000000606000000000000099900000000000099999000000
000000000000000000000000000000000000ddddddd0000000000000000000000000000000000000000000000000000000000000000000000000099990000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000099900000000000099900000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000099900000000000099000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000099900000000000090000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009999999900000000000000099900
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999999999999000000000099900900
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009009999999900900000099900099900
00080000000a00000000800000a00000000080000a000000000008000a00000000000080a0000000000000080000000009009999999900900000090099900900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000009009999999900900000099900000900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000900999999009000000090000000900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000006ddd8ddd0000000090999999090000000090000000900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000009999999900000000090000000900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000006ddd8ddd0000000000099990000000000090000999900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000009900000000000090009999900
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000009900000000099990009999900
00080000000a00000000800000a00000000080000a000000000008000a00000000000080a0000000000000080000000000000009900000000999990009999900
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999999000000999990000999000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000999999000000999990000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000009999999900000099900000000000
__label__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000004444444444444444444444444444444444444444444444444444444444444444444444400000000000000000000000000000
00000000000000000000000000044444444444444444444444444444444444444444444444444444444444444444444444440000000000000000000000000000
00000000000000000000000000044499949994494999444994499944449994499944499944999449994999449999944999440000000000000000000000000000
00000000000000000000000000044944449449494944949449494494449449494494944449444494444944944494449444440000000000000000000000000000
00000000000000000000000000044944449449494944949449494494449449494494944449444494444944944494449444440000000000000000000000000000
00000000000000000000000000044999949994494999449999494494449994499944999944994499994944944494444994440000000000000000000000000000
00000000000000000000000000044944449449494944949449494494449444494494944444449494444944944494444449440000000000000000000000000000
00000000000000000000000000044944449449494944949449494494449444494494944444449494444944944494444449440000000000000000000000000000
00000000000000000000000000044499949449494999449449494494449444494494499949994449994944944494449994440000000000000000000000000000
00000000000000000000000000044444444444444444444444444444444444444444444444444444444444444444444444440000000000000000000000000000
00000000000000000000000000004444444444444444444444444444444444444444444444444444444444444444444444400000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111144444444411111111441111114411111111444444114444441111111444444444111111114411111111444444441111110000000000000
00000000000001111149999999941111114994111149941111114999999449999994111111499999999411111149941111114999999994111110000000000000
00000000000001111149999999994111114994111149941111149999999999999999411111499999999941111149941111149999999994111110000000000000
00000000000001111149944444999411114994111149941111499944449999444499941111499444449994111149941111499944444441111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149944444999411114994111149941111499411114994111149941111499444449994111149941111499444444441111110000000000000
00000000000001111149999999994111114994111149941111499411114994111149941111499999999941111149941111499999999994111110000000000000
00000000000001111149999999994111114994111149941111499411114994111149941111499999999941111149941111499999999994111110000000000000
00000000000001111149944444999411114994111149941111499411114994111149941111499444449994111149941111499444444441111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149941111499411114994111149941111499411114994111149941111499411114994111149941111499411111111111110000000000000
00000000000001111149944444999411114999444499941111499411114994111149941111499444449994111149944444999944444441111110000000000000
00000000000001111149999999994111111499999999411111499411114994111149941111499999999941111149999999999999999994111110000000000000
00000000000001111149999999994111111149999999411111499411114994111149941111499999999411111149999999944999999994111110000000000000
00000000000001111144444444999411111114444499941111144111114994111114411111499444444111111149944444411444444441111110000000000000
00000000000001111111111111498c11111111111149941111111111114994111111111111499411111111111149941111111111111111111110000000000000
0000000000000111111111111188cc11111111111149941111111111114994111111111111499411111111111149941111111111111111111110000000000000
000000000000011111111111c8ccccc1111111111149941111111111114994111111111111499411111111111149941111111111111111111110000000000000
000000000000011111111911ccccccc1111111111149941111111111114994111111111111499411111111111149941111111111111911111110000000000000
0000000000000111111119111ccccccc444114444499994444444444449999444444444444999944444114444499994444411111111911111110000000000000
0000000000000111111155511cccccac999449999999999999999999999999999999999999999999999449999999999999941111199111111110000000000000
00000000000001111111ddd111ccaa99999999999994499999999999999449999999999999944999999999999994499999994111111991111110000000000000
00000000000001111111222111ca9944449999444441144444444444444114444444444444411444449999444441144444999411199111111110000000000000
00000000000001111111ddd111499411114994111111111111111111111111111111111111111111114994111111111111499411111991111110000000000000
00000000000001111111111111499411114994111111111111111111111111111111111111111111114994111111111111499411199111111110000000000000
00000000000001111111ddd111499411114994111111111111111111111111111111111111111111114994111111111111499411111991111110000000000000
00000000000001111111555111499411114994111111111111111111111111111111111111111111114994111111111111499411199111111110000000000000
00000000000001111111191111499411114994444441111111111444444114444444444444444111114999444444411111499411111911111110000000000000
00000000000001111111191111499411114999999994111111114999999449999999999999999411111499999999941111499411111911111110000000000000
00000000000001111111111111499411114999999999411111149999999999999999999999999411111499999999941111499411111111111110000000000000
00000000000001111111111111499411114994444499941111499944449999444449944444444111114999444444411111499411111111111110000000000000
00000000000001111111111111499411114994111149941111499411114994111149941111111111114994111111111111499411111111111110000000000000
00000000000001111111191111499411114994111149941111499411114994111149941111111111114994111111111111499411111911111110000000000000
00000000000001111111191111499411114994111149941111499411114994111149941111111111114994111111111111499411111911111110000000000000
00000000000001111111555111499411114994111149941111499411114994111149941111000111114994111111111111499411115551111110000000000000
00000000000001111111ddd11149941111499444449994111149941111499411114994111900091111499944444111111149941111ddd1111110000000000000
00000000000001111111555111499411114999999999411111499411114994111149941111000111111499999994111111499411112221111110000000000000
00000000000001111111ddd11149941111499999999941111149941111499411114994111900091111114999999941111149941111ddd1111110000000000000
00000000000001111111eee11149941111499444449994111149941111499411114994111100011111111444449994111caaaac1111111111110000000000000
00000000000001111111ddd11149941111499411114994111149941111499411114994111900091111111111114994111cccccc111ddd1111110000000000000
0000000000000111111155511149941111499411114994111149941111499411114994111100011111111111114994111cccccc1115551111110000000000000
0000000000000111111119111149941111499411114994111149941111499411114994111111111111111111114994111cccccc1111911111110000000000000
0000000000000111111119111149941111499411114994111149941111499411114994111100011111111111114994111cccccc1111911111110000000000000
0000000000000111111111111149941111499444449994111149994444999411114994111900091111144444449994111c8888c1111111111110000000000000
00000000000001111111111111499411114999999999411111149999999941111149941111000111114999999999411111499411111111111110000000000000
00000000000001111111191111499411114999999994111111149999999411111149941119000911114999999994111111499411111111111110000000000000
00000000000001111111191111499411114444444441111111499944444111111149941111000111111444444441111111499411111911111110000000000000
00000000000001111111555111499411111111111111111111499411111111111149941119000911111111111111111111499411111911111110000000000000
00000000000001111111ddd111499411111111111111111111499411111111111149941111000111111111111111111111499411199111111110000000000000
00000000000001111111222111499411111111111111111111499411111111111149941111111111111111111111111111499411111991111110000000000000
00000000000001111111ddd111499411111111111111111111499411111111111149941111111111111111111111111111499411199111111110000000000000
00000000000001111111111111499944444114444444444444999944444114444499994444411444444444444441144444999411111991111110000000000000
00000000000001111111ddd111149999999449999999999999999999999449999999999999944999999999999994499999994111199111111110000000000000
00000000000001111111555111149999999999999999999999944999999999999994499999999999999999999999999999994111111991111110000000000000
00000000000001111111191111499944449999444444444444411444449999444441144444999944444444444499994444999411199111111110000000000000
00000000000001111111191111499411114994111111111111111111114994111111111111499411111111111149941111499411111911111110000000000000
00000000000001111111111111499411114994111111111111111111114994111111111111499411111111111149941111499411111911111110000000000000
00000000000001111111111111499411114994111111111111111111114994111111111111499411111111111149941111499411111111111110000000000000
00000000000001111111111111499411114994111111111111dddd11114994111111111111499411111111111149941111499411111111111110000000000000
0000000000000111114444444499941111499944444441111dd55dd1114994444441111111499944444111111149941111499444444111111110000000000000
0000000000000111114999999999411111149999999994111d5555d1114999999994111111149999999411111149941111499999999411111110000000000000
0000000000000111114999999999411111149999999994111d5555d1114999999999411111149999999941111149941111499999999941111110000000000000
0000000000000111114994444499941111499944444441111dd55dd1114994444499941111499944449994111149941111499444449994111110000000000000
00000000000001111149941111499411114994111111111111dddd11114994111149941111499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411114994111111111111111111114994111149941111499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411114994111111111111111111114994111149941111499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411114994111111111111111111114994111149941111499411114994111149941111499411114994111110000000000000
00000000000001111149944444999411114994444444411111444411114994444499941111499444444994111149941111499444449994111110000000000000
00000000000001111149999999994111114999999999941114999941114999999999411111499999999994111149941111499999999941111110000000000000
00000000000001111149999999994111114999999999941114999941114999999994111111499999999994111149941111499999999941111110000000000000
00000000000001111149944444999411114994444444411111444411114994444441111111499444444994111149941111499444449994111110000000000000
00000000000001111149941111499411114994111111111111111111114994111111111111499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411114994111111111111111111114994111dd1111111499411114994111149941111499411114994111110000000000000
0000000000000111114994111149941111499411111111111111111111499411d55d111111499411114994111149941111499411114994111110000000000000
0000000000000111114994111149941111499411111111111111111111499411d55d111111499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411114999444444411111111111114994111dd11dd111499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411111499999999941111111111114994111111d55d11499411114994111149941111499411114994111110000000000000
00000000000001111149941111499411111149999999941111111111114994111111d55d11499411114994111149941111499411114994111110000000000000
000000000000011111144111111441111111144444444111111111111114411111111dd111144111111441111114411111144111111441111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000001111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111110000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000099999000999009000009900900900000000000009900099009009009990900009990000000000000000000000000000000
00000000000000000000000000000999999900900909000090090900900000000000009990999009009090000900009009000000000000000000000000000000
00000000000000000000000000000990009900900909000090090900900000000000000999990009009090000900009009000000000000000000000000000000
00000000000000000000000000000990009900999009000099990099900000000000000099900009999099990900009990000000000000000000000000000000
00000000000000000000000000000990009900900009000090090000900000000000000999990009009090000900009000000000000000000000000000000000
00000000000000000000000000000999999900900009000090090000900000000000009990999009009090000900009000000000000000000000000000000000
00000000000000000000000000000099999000900009999090090999000000000000009900099009009009990999909000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__map__
00000000000000001400000000160000000000000000001a1a1a1c0000000000001618001a0016001a001c14121a1a1a1a1a1a1a18000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
160000000000000015000000001500000000000000000000000011000000000000131c130015001a00160015000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000150000000011000000000000000000000000000000000000000019001900160019001c15000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
150000001f00000015001f00131a1c001f0000000019001300000000000019130000001500190016001a0015000000001f00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000150000000014000000000000001c00160000000000001c1600000015130015001c001c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000190000000015000000000000000000000000001600000000000000150019001c00150015000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
110000000000000000000000001900000000140000000000000000131c0000000000001113001a0013001300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
121a1a1a1c00000014161a1a1a1e1a1a1a1c131a1a1a00000000001219000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000151500000000000000150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000151500000000000000150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1600000017000000151700000017000000150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
150000001d00000019150000001d0000001d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000001500000000000000150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000001500000000000000150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
11000000131a1a1a18131a1a1a1b1a1a1a190000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
211c1414161e1c211c141618121a1a1a1a1a1a1a18161a1a1a1a1a1a1a1c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
241d1515151515241d1524181618161c161e1c16181512251814141618150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
201d131d1115112419241b181514242615151524181500150024262418150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00171e1b1a1b1a1b1e1b1c001319111111111113181500110011111318150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015241c161e251817181500161c14141618211c1415001618211c211c150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015241d15151500131c1500151515152418241d111500241815151515150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015201917191500121915001319201913181111101500131811112019150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00171e1a1b1e1b1e1a1e1d00121a1a1a1a1a1a1a18131a1a1a1a1a1a1a190000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
211d171800241c171c15241c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
241d2418102419242615241d0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1111131800110011111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
211c1414161e1c211c14161800211c161e251e180000211c161800211c161c14211c00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
241d1515151515241d15241800241d151515131c0000241d2418102419242615241d00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
201d131d1115112419241b180024191319151219000015111718001500151115151100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
121b1a1b1a1b1a1b1a1b1a1a1a1b1a1a1a1b1a1a1a1a1b1a1b1a1a1b1a1b1a1b1b1800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
__sfx__
000400003172438723001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100001000010000100
000100001917011170091630050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500005000050000500
00040000140701f070000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01020000256402564024630216301d620156200d6200a61006610056100361503600026051a6001960015600116000e6000860003600026050060000600006000060000600006000060000600006000060000600
01080000117501175011750000001075010750107501c7000e7500e7500e750007000c7500c7500c7500c7500c7500c7500c75000700007000070000700007000070000700007000070000700007000070000700
00090000137241272111721107210f7210e7210d7210c7210b7210a72109721087110771100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000200001d02000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000300001813018131181311813500100001001c1401c1411c1411c1311d1311d1311d1311d1211d1211d1251d100001000010000100001000010000100001000010000100001000010000100001000010000100
0106000015150181411a1311a1211a1151710016100101501115112141131311312113115131000010009150091510a1410a1410b1310b1310d1210d1210d1110d11500100001000010000100001000010000100
010a00001c7501c7501c7501c7551d7501d7501d7501d7551f7621f7621d7521d7521f7521f7521f7521f75500700007000070000700007000070000700007000070000700007000070000700007000070000700
011000000555005550055500555504550045500455204555025500255202552025550055200552005520055200552005520055500500005000050000500005000050000500005000050000500005000050000500
010a00002c757327572c7573575500700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700007000070000700
010a0000116301062009613006000b6000b6000860006600046000260002600000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
010800002473003700107001a7002a7002a70020705207052970020700237002c7012a7002a70020705207052470028700067052c7002a7002a70020705207052470020700237002c7012a7002a7002070520705
010800001f7301f705297052570528704287052670426705007050070500705007050070500705007050070500705007050070500705007050070500705007050070500705007050070000700007000070000700
001000000000000000000000000000000000000000000000000000000000000000001005000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000180500c050130400c0501b5403f0001b51012000180400c040130400c0401b5403f0001b51012000180500c050130600c0601b5400c050180500c050180200c050130500c0501b5500c0001b5100c000
011000001b0500f050160400f0511e532240031e512240051b0500f050160400f0511e532210001e512220001b0500f050160400f0511e5320f050160400f0511b0500f050160400f0511e532210001e51222000
012000002b0402b0402b0402b0422b0422b0422b0422c0402b0402b0422b0422b042270402704027042240402a0402a0402a0312a0312a0212a0212a0212a0112a0112a0112a0152a00522000220022200224000
011000002a0402a0402a0312a0312a0212a0212a0212a0112a0112a0112a0152a0052200022002220022400000000000000000000000000000000000000000000000000000000000000000000000000000000000
012000002b0402b0312b0312b0322b0322b0322b03232040300403003130032300322b0402b0312b0322b0322e0402e0412e0312e0312e0212e0212e0212e0112e0112e0112e0112e0153000230002300022b000
011000002e0402e0412e0312e0312e0212e0212e0212e0112e0112e0112e0112e0153000230002300022b00000000000000000000000000000000000000000000000000000000000000000000000000000000000
011000001f100241002712026120241201f120181200010000100001002b120271202612024120201201f12027100261002712026120241201f120181200010000100001002b120271202612024120201201f120
0110000000100001002a1202912027120221201b1200010000100001002e1202a1202912027120231202212000100001002a1202912027120221201b1200010000100001002e1202a12029120271202312022120
011000001f110241102713026130241301f13018130131101f110241102b130271302613024130201301f1301f110241102713026130241301f13018130131101f110241102b130271302613024130201301f130
0110000022110271102a1302913027130221301b1301611022110271102e1302a1302913027130231302213022110271102a1302913027130221301b1301611022110271102e1302a13029130271302313022130
011000000c073000030c0531800318655180030c0530c0530c073000030c05318003186550c0530c043000430c073000030c0531800318655180030c0530c0530c073000030c05318003186550c0530c04300043
011000000c073000030c0031861518655180030c0030c0530c003000030c0531800318655000430c003000430c073000030c003186151865518625186150c0530c003000030c0531800318615186251863518655
__music__
00 10424344
00 11424344
00 11424344
00 10124344
00 11134344
00 10144344
00 11154344
01 10164344
00 11174344
01 10184344
00 11194344
00 10181244
00 11191344
00 10181444
00 11191544
00 1018431a
00 1119431a
00 1052431a
00 1052431b
00 1052121a
00 1153131a
00 1052141a
00 1153151a
00 4118431a
02 4118431a
00 11424344
00 11424344
01 10124344
00 11134344
00 10144344
02 11154344

