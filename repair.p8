pico-8 cartridge // http://www.pico-8.com
version 18
__lua__
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

digits={
 125,80,55,87,90,79,111,81,127,95
}
function draw_digit(digit,x0,y0)
 local v=digits[digit+1]
 for i=0,2 do
  if band(v,shl(1,i))!=0 then
   line(x0+1,y0+4*i,x0+3,y0+4*i)
  end
 end
 for j=0,3 do
  if band(v,shl(1,3+j))!=0 then
   local x=x0+4*(j%2)
   local y=y0+4*flr(j/2)+1
   line(x,y,x,y+2)
  end
 end
end

function draw_number(
 v,x,y,num_digits
)
 for i=1,num_digits do
  local digit=v%10
  v=(v-digit)/10
  draw_digit(
   digit,x+(num_digits-i)*7,y
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
  spr(48,96,8,4,1) --timed out
  return
 end

 local i=1
 local x=127
 while (time_left>0) do
  local tb=timebar[i]
  local l=min(
   tb[1],time_left/tb[2]
  )-1
  rectfill(x-l,9,x,11,tb[3])
  line(x-l,12,x,12,tb[4])
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

function switch_music(track)
 if track!=current_track then
  music(track)
  current_track=track
 end
end
-->8
-- tiles
gridtile={}

_nxt_tile_idx=0
function gridtile:new(
 entries,connections,prob
)
 local o=setmetatable({},self)
 self.__index=self

 o.entries=entries
 o.connections=connections
 o.prob=prob

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
 gridtile:new( 3,{ 2, 1, 0, 0},0),
 gridtile:new( 6,{ 0, 4, 2, 0},0),
 gridtile:new(12,{ 0, 0, 8, 4},0),
 gridtile:new( 9,{ 8, 0, 0, 1},0),
 gridtile:new( 7,{ 4, 5, 1, 0},0),
 gridtile:new(14,{ 0, 8,10, 2},0),
 gridtile:new(13,{ 4, 0, 1, 5},0),
 gridtile:new(11,{10, 8, 0, 2},0)
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
   screen_pos.x,screen_pos.y,
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
 o.x0=64-flr(o.w*tilesize/2)+0.5
 o.y0=64-flr(o.h*tilesize/2)+0.5

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

function tilegrid:_pos2idx(pos)
 return pos.x+pos.y*self.w
end

function tilegrid:_idx2pos(idx)
 return vector:new(
  idx%self.w,flr(idx/self.w)
 )
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
 printh("releasing "..pos:to_string())
 if t.bot==nil then
  printh("already nil")
 end
 assert(t.bot==bot)
 t.bot=nil
 self:dump_claimed()
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
  self:dump_claimed()
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

function move_straight(bot)
 local dr=vdirs[bot.dir]

 return function()
  for i=1,12 do
   if i==9 then
    while bot:_is_blocked() do
     yield()
    end
   end

   bot.dirv:add(dr)

   if i==4 then
    bot:_release_prv()
   end

   yield()
  end
 end
end

function move_reverse(bot)
 local dr=vdirs[bot.dir]

 return function()
  --move halfway
  for i=1,6 do
   bot.dirv:add(dr)
   if i==4 then
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
  for i=1,6 do
   if i==3 then
    while bot:_is_blocked() do
     yield()
    end
   end
   bot.dirv:sub(dr)
   yield()
  end
 end
end

function move_turn(bot)
 local dr1=vdirs[bot.dir]
 local dr2=vdirs[bot.nxt_dir]

 return function()
  local o=dr2:orientation(dr1)

  --initial straight bit
  for i=1,3 do
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
   bot.dirv:add(dr1)
   bot.dirv:add(dr2)
   yield()
  end

  --final straight bit
  bot:_delta_rot(o)
  for i=1,3 do
   bot.dirv:add(dr2)
   yield()
  end
 end
end

function crash_anim(bot)
 local dr=vdirs[bot.nxt_dir]

 return function()
  --falling forward
  for i=1,4 do
   bot.crash_sprite+=2
   bot.dirv:add(dr)
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

  local v=0
  while true do
   v+=rnd(1)
   if flr(v)%3==0 then
    bot.rearlightcolor=9
   else
    bot.rearlightcolor=8
   end
   yield()
  end
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
  yield()

  bot.period=10 --fix speed

  --wiggle
  while true do
   bot:_delta_rot(1)
   yield()
   bot:_delta_rot(-1)
   yield()
   bot:_delta_rot(-1)
   yield()
   bot:_delta_rot(1)
   yield()
  end
 end
end

function bot:new(pos,dr0,o)
 o=setmetatable(o or {},self)
 self.__index=self

 o.period=30
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
 o.on_paired={}

 return o
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
 self.rearlightcolor=8
 self.period=6 --fixed speed
 sfx(5) --falling

 self.update_cr=cowrap(
  "crash_anim",
  crash_anim(self)
 )
end

function bot:_set_move_anim()
 if self.dir==self.nxt_dir then
  self.update_cr=cowrap(
   "move_straight",
   move_straight(self)
  )
 elseif abs(
  self.dir-self.nxt_dir
 )==2 then
  self.update_cr=cowrap(
   "move_reverse",
   move_reverse(self)
  )
 else
  self.update_cr=cowrap(
   "move_turn",
   move_turn(self)
  )
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
 self.dirv=dirv*6
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

function bot:_will_pair_with(
 bot2
)
 printh("will_pair_with check")
 self:dump()
 printh(" with ")
 bot2:dump()
 printh("?")

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
 if self.pairing!=nil then
  --never blocked when pairing
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
  self:_will_pair_with(claimer)
 ) then
  self.pairing=claimer
  claimer.pairing=self
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
 self.update_cr=cowrap(
  "pair_anim",
  pair_anim(self,self.pairing)
 )
 self.pairing=nil
end

function bot:stop()
 self.update_cr=nil
 self.pairing=nil
end

function bot:update()
 self.clk=(self.clk+1)%self.period
 if self.clk>0 then
  return
 end

 if (
  self.update_cr!=nil and
  coinvoke(self.update_cr)
 ) then
  self:_move_step()
 end

 if self.pairing!=nil then
  local pbot=self.pairing
  local p1=grid:screen_pos(
   self.pos
  )+self.dirv
  local p2=grid:screen_pos(
   pbot.pos
  )+pbot.dirv
  printh("dist="..p1:dist(p2))
  if p1:dist(p2)<=10.5 then
   self.pairing:_paired()
   self:_paired()
   fire_event(
    self.on_paired,self
   )
  end
 end
end

function bot:draw()
 local pos=grid:screentile_at(
  self.pos
 ).pos
 if self.crashing then
  pal(8,self.rearlightcolor)
  spr(
   self.crash_sprite,
   pos.x+self.dirv.x-1,
   pos.y+self.dirv.y-1,
   2,2,
   self.flipx,self.flipy
  )
 else
  local si=self.rot%8
  if self.rot>7 then
   --invert rear/front lights
   pal(8,10)
   pal(10,8)
  end
  spr(
   160+si*2,
   pos.x+self.dirv.x-1,
   pos.y+self.dirv.y-1,
   2,2
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

 o.cursor_pos=vector:new(o.x0,0)

 o.tiles={}
 o.num_tiles=0
 while (
  o.num_tiles<size and
  tiletray._replenish(o)
 ) do end

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
   self.x0+(i-1)*mul,0
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

function tiletray:_replenish()
 assert(self.num_tiles<self.size)

 local tile=self:_new_tile()
 if tile==nil then
  --no new tile is placeable
  return false
 end

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
 if (
  self.update_cr!=nil and
  coinvoke(self.update_cr)
 ) then
  self.update_cr=nil
 end
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
--cursor

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

 if btnp(🅾️) then
  if tray.num_tiles>=2 then
   printh("tray switch")
   tray:switch()
   self:_check_allowed()
  else
   sfx(1) --no can do
  end
 end

 if btnp(❎) then
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
  draw_tile(
   tray:selected_tile(),
   self.draw_pos
  )
  pal()
 end

 draw_cursor(
  self.draw_pos,self.contraction
 )
end

-->8
--game

function new_game()
 level=1
 numlives=3
 score=0
 draw_score=score
 speedup_cr=nil
 start_level()
end

function load_level()
 local lspec=levelspecs[level]

 local gs=lspec.grid
 grid=tilegrid:new(
  13,gs[1],gs[2],gs[3],gs[4]
 )

 bots={}
 for bs in all(lspec.bots) do
  local bot=bot:new(
   vector:new(bs[1],bs[2]),
   bs[3]
  )
  if bs[4] then
   bot.period=bs[4]
  end

  add(bots,bot)
  add(bot.on_move,on_move)
  add(bot.on_crash,on_crash)
  add(bot.on_paired,on_paired)
 end

 tray=tiletray:new(3)
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
  1,"end attempt",on_abort
 )
 menuitem(
  2,"end game",end_game
 )

 switch_music(0)
end

function start_level()
 local p=vector:new(48,0)
 local tp=vector:new(48,50)
 local t=0

 switch_music(-1)

 _draw=function()
  cls()
  color(4)
  roundrect(46,p.y-1,81,p.y+9)
  color(9)
  drawtext(
   "level "..level,p.x,p.y
  )
  if t>30 then
   color(9)
   drawtext("get ready!",41,62)
  end
 end
 _update60=function()
  p:lerp(tp,0.1)
  t+=1
  if t==30 then
   sfx(7)
  end
  if t==90 or action_btnp() then
   load_level()
  end
 end
end

function next_level()
 level+=1
 if level<=#levelspecs then
  start_level()
 else
  morph_grid(39)
  sleep(30,true)
  mainmenu()
 end
end

function update_game()
 grid:update()
 foreach(bots,bot.update)
 if curs!=nil then
  curs:update()
 end
 if tray!=nil then
  tray:update()
 end

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
 for i=1,numlives do
  spr(12,128-i*8,0)
 end

 color(4)
 draw_number(draw_score,0,2,5)

 draw_timebar(
  ticks_remaining/fps
 )
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
 for bot2 in all(bots) do
  if (
   bot!=bot2 and (
    (
     bot.pos==bot2.pos and
     bot.dir!=bot2.dir
    ) or (
     bot.pos==bot2.nxt_pos and
     bot.nxt_pos==bot2.pos
    )
   ) and
   not bot2.crashing
  ) then
   bot.pairing=bot2
   bot2.pairing=bot
  end
 end
end

function on_paired(bot1)
 printh("bots paired")

 end_play()
 speedup_cr=nil
 end_anim=cowrap(
  "level_done_anim",
  level_done_anim
 )
end

function on_crash(bot)
 printh("bot crashed")

 sfx(4)
 on_death()
end

function on_abort()
 sfx(8)
 on_death()
end

function end_play()
 switch_music(-1)

 --remove menu items
 menuitem(1)
 menuitem(2)

 disable_input()
end

function on_death()
 end_play()

 if numlives>0 then
  numlives-=1
  end_anim=cowrap(
   "retry_anim",retry_anim
  )
 else
  gameover(2)
 end
end

function end_game()
 end_play()
 gameover()
end

function bot_at(pos)
 for bot in all(bots) do
  if bot.pos==pos then
   return true
  end
 end
 return false
end

function gameover(wait)
 printh("game over")
 end_anim=cowrap(
  "gameover_anim",
  function()
   sleep(wait or 0)
   gameover_anim()
  end
 )
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
   if bot.period>1 then
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

 score+=100
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

 sleep(2,true)
 next_level()
end

function gameover_anim()
 morph_grid(12)
 mainmenu()
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

 for i=1,grid.h*grid.w do
  local p=grid:_idx2pos(i-1)
  if (
   grid:screentile_at(
    p
   ).tile==target_tile(p)
  ) then
   --it's already correct
   add(tile_status,2)
  elseif (
   grid:tile_at(p)!=nil or
   grid:has_neighbour(p)
  ) then
   --it can be updated
   add(tile_status,1)
   add(ready,i)
  else
   --this tile is empty. wait
   --until it has at least one
   --neighbour
   add(tile_status,0)
  end
 end

 local place_tile=function(pos)
  local tile=target_tile(pos)
  grid:place_tile(
   screentile:new(tile),pos,true
  )
 end

 local update_tile=function(pos)
  local i=grid:_pos2idx(pos)+1
  local status=tile_status[i]
  if status==0 then
   tile_status[i]=1
   add(ready,i)
  end
  if status!=2 then
   local tile=grid:patch_tile(
    pos
   )
   if (
    tile==target_tile(pos)
   ) then
    --no further update needed
    if status==1 then
     del(ready,i)
    end
    tile_status[i]=2
   end
  end
 end

 local anim=function()
  while #ready>0 do
   local i=rnd_item_from(ready)
   del(ready,i)
   local p=grid:_idx2pos(i-1)
   --printh("popped "..p:to_string())
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

  sleep(3,true)
 end

 anim()
end
-->8
-- main

levelspecs={
{--easy win
 grid={9,7,0,7},
 bots={{1,0,2},{7,0,4}},
 misc={30}
},{--level1
 grid={9,7,0,0},
 bots={{0,6},{8,0}},
 misc={180}
},{--level2
 grid={9,7,9,0},
 bots={{4,0,4,60},{4,6,2,60}},
 misc={240}
}}

function _init()
 printh("---- init ----")
 mainmenu()
end

function mainmenu()
 switch_music(25)
 grid=tilegrid:new(7,12,11,0,16)
 local clk=0
 local buttons_enabled=false

 _update60=function()
  clk+=1
  if clk==fps then
   buttons_enabled=true
  end

  grid:update()
  if buttons_enabled then
   if btnp(🅾️) then
    new_game()
   end
   if btnp(❎) then
    showhelp()
   end
  end
 end

 _draw=function()
  cls()

  grid:draw()

  color(4)
  roundrect(27,7,99,17)
  color(9)
  drawtext(
   "eriban presents",28,8
  )

  if buttons_enabled then
   spr(10,29,111)
   drawtext("play",37,110)

   spr(11,70,111)
   drawtext("help",78,110)
  end
 end
end

function showhelp()
 _update60=function()
  if action_btnp() then
   mainmenu()
  end
 end

 _draw=draw_help
end

function draw_help()
 cls()

 palt(7,true)
 for x=12,19 do
  for y=23,25 do
   v=mget(x,y)
   if v>0 then
    spr(v,x*7-50,y*7-161)
   end
  end
 end

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
__gfx__
000000000000000000c00000000c00000000c0000000000000c00000000c00000000c00066600000099999009900099004404400000000000000000000000000
000000000caaac0000caa00000cca00000cca0000ccccc00008cc000008cc0000088c00060000000999999909990999044444440000000000000000000000000
007007000ccccc000ccccac00cccca00ccccca0008ccca0008ccccc008cccc00c8cccc0060000000990009900999990044444440000000000000000000000000
000770000ccccc000ccccc00ccccccc008ccca0008ccca0008ccca00ccccccc00ccccc0000000000990009900099900044444440000000000000000000000000
000770000ccccc00c8cccc0008cccc0008ccccc008ccca00ccccca000cccca000ccccac000000000990009900999990004444400000000000000000000000000
007007000c888c000088c000008cc000008cc0000ccccc0000cca00000cca00000caa00000000000999999909990999000444000000000000000000000000000
00000000000000000000c000000c000000c00000000000000000c000000c000000c0000000000000099999009900099000040000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
77777777779a977777777777779a977777777777779a977777777777779a977777777777779a977777777777779a977777777777779a977777777777779a9777
77777777779a977777777777779a977777777777779a977777777777779a977777777777779a977777777777779a977777777777779a977777777777779a9777
77999777779a9777777999977779a99777797777779a9777777779977779a9979999777799a977779999999799a9a9979977777799a9777799777997999a9997
79aaa977779a9777779aaaa777779aa7779a9777779a977777779aa777779aa7aaaa9777aa977777aaaaaaa7aa979aa7aa977777aa977777aa979aa7aaaaaaa7
77999777777977777779999777777997779a9777779a97777779a9977779a9979999777799777777999999979977799799a9777799a9777799a9a997999a9997
77777777777777777777777777777777779a9777779a9777779a9777779a977777777777777777777777777777777777779a9777779a9777779a9777779a9777
77777777777777777777777777777777779a9777779a9777779a9777779a977777777777777777777777777777777777779a9777779a9777779a9777779a9777
77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
779a97777777777777777777779a9777779a977777777777779a9777779a97770000000000000000000000000000000000000000000000001111111111111777
779a97777777777777777777779a9777779a977777777777779a9777779a97770000000000000000000000000000000000000000000000001111111111111777
779a99977799999799999777999a9777779a999799999997999a9777999a99970000000000000000000000000000000000000000000000001111111111111777
779aaaa7779aaaa7aaaa9777aaaa9777779aaaa7aaaaaaa7aaaa9777aaaaaaa70000000000000000000000000000000000000000000000001111111111111777
77999997779a9997999a977799999777779a9997999a9997999a9777999999970000000000000000000000000000000000000000000000001111111111111777
77777777779a9777779a977777777777779a9777779a9777779a9777777777770000000000000000000000000000000000000000000000001111111111111777
77777777779a9777779a977777777777779a9777779a9777779a9777777777770000000000000000000000000000000000000000000000001111119111111777
77777777777777777777777777777777777777777777777777777777777777770000000000000000000000000000000000000000000000001111111111111777
99909090009099909900009009090999000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111777
09009099099090009090090909090090000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111777
09009090909099009090090909090090000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111777
09009090009090009090090909090090000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111777
09009090009099909900009000990090000000000000000000000000000000000000000000000000000000000000000000000000000000001111111111111777
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007777777777777777
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007777777777777777
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000007777777777777777
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000000000000000000000000000000000000a0000000000000000000000000000000000000000000000000000000000000008000000000000000000000000
000000000000000000000aa000000000000000aca000000000000000aa000000000000000000000000000880000000000000008c800000000000000088000000
000aaaaaaaaa000000000acaa0000000000000ccca0000000000000cca0000000008800000aa0000000008cc00000000000008ccc000000000000088c8000000
000accccccca000000000ccccaa0000000000ccccca0000000000ccccca000000008ccccccca000000008ccccc00000000008ccccc000000000088cccc000000
0000ccccccc000000000cccccccaa0000000ccccccca00000088cccccca000000008ccccccca000000008ccccccaa0000008ccccccc000000088ccccccc00000
0000ccccccc000000000cccccccca000008ccccccccca000008cccccccca00000008ccccccca00000008cccccccca000008ccccccccca000008cccccccc00000
0000ccccccc00000000ccccccccc000008ccccccccccca000008ccccccca00000008ccccccca00000008ccccccca000008ccccccccccca00000ccccccccc0000
0000ccccccc00000008cccccccc00000008ccccccccca0000008cccccccca0000008ccccccca0000008cccccccca0000008ccccccccca0000000cccccccca000
0000ccccccc000000088ccccccc000000008ccccccc0000000008ccccccaa0000008ccccccca00000088cccccca000000000ccccccca00000000cccccccaa000
0008ccccccc80000000088cccc00000000008ccccc00000000008ccccc0000000008ccccccca000000000ccccca0000000000ccccca0000000000ccccaa00000
000888888888000000000088c8000000000008ccc0000000000008cc000000000008800000aa00000000000cca000000000000ccca00000000000acaa0000000
000000000000000000000000880000000000008c800000000000088000000000000000000000000000000000aa000000000000aca000000000000aa000000000
0000000000000000000000000000000000000008000000000000000000000000000000000000000000000000000000000000000a000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000accccccca00000000ccccccc000000000ccccccc000000000ccccccc000000000ccccccc00000000000000000000000000000000000000000000000000000
0000ccccccc00000000accccccca00000000ccccccc000000000ccccccc000000000ccccccc000000000ddddddd0000000000000000000000000000000000000
0000ccccccc000000000ccccccc00000000accccccca0000000accccccca00000000ddddddd000000000ddddddd0000000000000000000000000000000000000
0000ccccccc000000000ccccccc000000000ccccccc000000000ddddddd00000000addddddda00000000ddddddd0000000000000000000000000000000000000
0000ccccccc000000000ccccccc000000000ccccccc000000000ddddddd000000000ddddddd00000000888888888000000000000000000000000000000000000
0000ccccccc000000000ccccccc000000000ddddddd000000000ddddddd0000000088888888800000000ddddddd0000000000000000000000000000000000000
0000ccccccc000000000ddddddd000000000ddddddd0000000088888888800000000ddddddd000000000ddddddd0000000000000000000000000000000000000
0000ddddddd00000000888888888000000088888888800000000ddddddd000000000ddddddd000000000ddddddd0000000000000000000000000000000000000
00088888888800000000ddddddd000000000ddddddd000000000ddddddd000000000ddddddd00000000000606000000000000000000000000000000000000000
000000000000000000000000000000000000ddddddd0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00080000000a00000000800000a00000000080000a000000000008000a00000000000080a0000000000000080000000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000006ddd8ddd0000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000006ddd8ddd0000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000000000000000000000000000000
0008dccccccc0000000d8dcccccc000000dd8ddccccc0000000dd8dddccc0000000ddd8dddcc00000000ddd8ddd0000000000000000000000000000000000000
00080000000a00000000800000a00000000080000a000000000008000a00000000000080a0000000000000080000000000000000000000000000000000000000
__label__
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000099999990000000090000009000000009999000999900000009999999000000009000000009999990000000000000000000000000
0000000000000000000000009aaaaaa90000009a900009a90000009aaaa909aaaa90000009aaaaaa90000009a90000009aaaaaa9000000000000000000000000
0000000000000000000000009a99999a9000009a900009a9000009a9999a9a9999a9000009a99999a9000009a9000009a9999990000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a99999a9000009a900009a900009a900009a900009a900009a99999a9000009a900009a99999990000000000000000000000000
0000000000000000000000009aaaaaa90000009a900009a900009a900009a900009a900009aaaaaa90000009a900009aaaaaaaa9000000000000000000000000
0000000000000000000000009a99999a9000009a900009a900009a900009a900009a900009a99999a9000009a900009a99999990000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a900009a900009a900009a900009a900009a900009a900009a900009a900009a900009a90000000000000000000000000000000
0000000000000000000000009a99999a90000009a9999a9000009a900009a900009a900009a99999a9000009a99999a9a9999990000000000000000000000000
0000000000000000000000009aaaaaa9000000009aaaa90000009a900009a900009a900009aaaaaa90000009aaaaaa909aaaaaa9000000000000000000000000
0000000000000000000000009999999a9000000009999a90000009000009a9000009000009a9999900000009a999990009999990000000000000000000000000
00000000000000000000000000000009a9000000000009a9000000000009a9000000000009a9000000000009a900000000000000000000000000000000000000
00000000000000000000000000000009a9000000000009a9000000000009a9000000000009a9000000000009a900000000000000000000000000000000000000
00000000000000000000000000000009a9000000000009a9000000000009a9000000000009a9000000000009a900000000000000000000000000000000000000
00000000000000000000000000000009a9000000000009a9000000000009a9000000000009a9000000000009a900000000000000000000000000000000000000
000000000000000000000000000000009a99990009999a9a99999999999a9a99999999999a9a99990009999a9a99990000000000000000000000000000000000
0000000000000000000000000000000009aaaa909aaaa909aaaaaaaaaaa909aaaaaaaaaaa909aaaa909aaaa909aaaa9000000000000000000000000000000000
000000000000000000000000000000009a9999a9a999900099999999999000999999999990009999a9a99990009999a900000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000000000000000000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000000000000000000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000000000000000000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000000000000000000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a999990000000000999900099999999999990000009a999999000009a90000000000000000000000000000000
00000000000000000000000000000009a900009aaaaaa9000000009aaaa909aaaaaaaaaaaaa90000009aaaaaa900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a99999a90000009a9999a9a99999a99999990000009a999999000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a9000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a9000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a9000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a9000000000009a9000000000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a99999a9000009a900009a900009a90000000000009a999900000009a90000000000000000000000000000000
00000000000000000000000000000009a900009aaaaaa90000009a900009a900009a900000000000009aaaa90000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a99999a9000009a900009a900009a9000000000000009999a9000009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a90000000000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a90000000000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a90000000000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a900009a900009a900009a900009a90000000000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a99999a90000009a9999a9000009a9000000000000999999a9000009a90000000000000000000000000000000
00000000000000000000000000000009a900009aaaaaa9000000009aaaa90000009a9000000000009aaaaaa90000009a90000000000000000000000000000000
00000000000000000000000000000009a900009999999000000009a999900000009a900000000000099999900000009a90000000000000000000000000000000
00000000000000000000000000000009a90000000000000000009a9000000000009a900000000000000000000000009a90000000000000000000000000000000
00000000000000000000000000000009a90000000000000000009a9000000000009a900000000000000000000000009a90000000000000000000000000000000
00000000000000000000000000000009a90000000000000000009a9000000000009a900000000000000000000000009a90000000000000000000000000000000
00000000000000000000000000000009a90000000000000000009a9000000000009a900000000000000000000000009a90000000000000000000000000000000
000000000000000000000000000000009a999900099999999999a9a99990009999a9a9999000999999999990009999a900000000000000000000000000000000
0000000000000000000000000000000009aaaa909aaaaaaaaaaa909aaaa909aaaa909aaaa909aaaaaaaaaaa909aaaa9000000000000000000000000000000000
000000000000000000000000000000009a9999a9a999999999990009999a9a99990009999a9a99999999999a9a9999a900000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000009a9000000000009a9000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000009a9000000000009a9000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000009a9000000000009a9000000000009a900009a90000000000000000000000000000000
00000000000000000000000000000009a900009a90000000000000000009a9000000000009a9000000000009a900009a90000000000000000000000000000000
0000000000000000000000009999999a90000009a9999990000000000009a99999000000009a999900000009a900009a99999000000000000000000000000000
0000000000000000000000009aaaaaa9000000009aaaaaa9000000000009aaaaaa9000000009aaaa90000009a900009aaaaaa900000000000000000000000000
0000000000000000000000009a99999a90000009a9999990000000000009a99999a90000009a9999a9000009a900009a99999a90000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a900009a900009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a900009a900009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a900009a900009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a900009a900009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a99999a9000009a99999990000099900009a99999a9000009a999999a900009a900009a99999a90000000000000000000000000
0000000000000000000000009aaaaaa90000009aaaaaaaa90009aaa90009aaaaaa90000009aaaaaaaa900009a900009aaaaaa900000000000000000000000000
0000000000000000000000009a99999a9000009a99999990000099900009a9999900000009a999999a900009a900009a99999a90000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a9000000000009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a9000000000009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a9000000000009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a900009a90000000000000000009a9000000000009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a9000009a9999990000000000009a9000000000009a900009a900009a900009a900009a9000000000000000000000000
0000000000000000000000009a900009a90000009aaaaaa9000000000009a9000000000009a900009a900009a900009a900009a9000000000000000000000000
00000000000000000000000009000000900000000999999000000000000090000000000000900000090000009000000900000090000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000

__map__
0000000000000000140000000016000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1600000000000000150000000015000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000150000000011000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
150000001f00000015001f00131a1c001f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000150000000014000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1500000000000000190000000015000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1100000000000000000000000019000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
121a1a1a1a1a1a1a180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
000000001f000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
211c1414161e1c211c1416181618161c161e1c1618141414161814001400122518141416180000121a251a1a1a1a1a180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
241d1515151515241d15241815142426151515241815151524181500150014150024262418161c0000150014140016180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
201d131d1115112419241b18131911111111111318131b191318201820181511001111131815150000150024260024180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00171e1b1a1b1a1b1e1b1c00000000000000000000000000000000000000171a1a1a1a1a1a26150000110011110013180000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015241c161e251817181500161c14141618211c14211c161c211c161814151618211c211c151500161800211c00211c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015241d15151500131c1500151515152418241d111515151515152418111124181515151513190024180015150015150000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
0015201917191500121915001319201913181111102019131911111318100013181111201900000013180011110020190000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
00171e1a1b1e1b1e1a1e1d00141416181400211c000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
211d171800241c171c15241c2426241815002419000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
241d2418102419242615241d1111131820181100000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
1111131800110011111111110000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
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
001000001f000210001f0001a00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000010050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000100500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000001005000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000010050000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000100500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
001000000000000000000000000000000000000000000000000000000000000000001005000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
01100000180500c050130400c0501b5403f0001b51012000180400c040130400c0401b5403f0001b51012000180500c050130600c0601b5400c050180500c050180200c050130500c0501b5500c0001b5100c000
011000001b0500f050160400f0511e532240031e512240051b0500f050160400f0511e532210001e512220001b0500f050160400f0511e5320f050160400f0511b0500f050160400f0511e532210001e51222000
012000002b0402b0402b0402b0422b0422b0422b0422c0402b0402b0422b0422b042270402704027042240402a0402a0402a0312a0312a0212a0212a0212a0112a0112a0112a0152a00522000220022200224000
011000002a0402a0402a0312a0312a0212a0212a0212a0112a0112a0112a0152a0052200022002220022400000000000000000000000000000000000000000000000000000000000000000000000000000000000
012000002b0402b0312b0312b0322b0322b0322b03232040300403003130032300322b0402b0312b0322b0322e0402e0412e0312e0312e0212e0212e0212e0112e0112e0112e0112e0153000230002300022b000
011000002e0402e0412e0312e0312e0212e0212e0212e0112e0112e0112e0112e0153000230002300022b00000000000000000000000000000000000000000000000000000000000000000000000000000000000
011000201f100241002712026120241201f120181200010000100001002b120271202612024120201201f12027100261002712026120241201f120181200010000100001002b120271202612024120201201f120
0110002000100001002a1202912027120221201b1200010000100001002e1202a1202912027120231202212000100001002a1202912027120221201b1200010000100001002e1202a12029120271202312022120
011000201f110241102713026130241301f13018130131101f110241102b130271302613024130201301f1301f110241102713026130241301f13018130131101f110241102b130271302613024130201301f130
0110002022110271102a1302913027130221301b1301611022110271102e1302a1302913027130231302213022110271102a1302913027130221301b1301611022110271102e1302a13029130271302313022130
011000200c073000030c0531800318655180030c0530c0530c073000030c05318003186550c0530c043000430c073000030c0531800318655180030c0530c0530c073000030c05318003186550c0530c04300043
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

