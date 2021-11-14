require daslib/media
require daslib/decs_boost
require daslib/json
require daslib/json_boost
require daslib/fio

var debug_text_buffer :array<tuple<string;float>>
let DEBUG_TEXT_TIME = 3.

def debug_print(text: string)
    debug_text_buffer |> push([[auto text, get_time_after_start()]])

let
    CELL_SIZE = 32

let
    ANIM_HERO_WALK_SPEED = 1.5
    HERO_IMG_SIZE = int2(16,45)
    HERO_IMG_OFFSET = float2(-float(HERO_IMG_SIZE.x)/2.0, -float(HERO_IMG_SIZE.y)+4.0)
    CELL_IMG_SIZE = float2(45,64)

let
    MENU_WIDTH = 200
    MENU_PADDING = 10
    MENU_BUTTON_HEIGHT = 64
    MENU_TEXT_SECTION_HEIGHT = 150
    MENU_BUTTON_TEXT_PADING = 20.0
    MENU_BUTTON_SECTION_OFFSET_Y = MENU_TEXT_SECTION_HEIGHT + 2*MENU_PADDING

var hero_img : ImageHandle
var interact_img : ImageHandle
var walls_img : ImageHandle

var text_buffer : array<string>

let
    WALL_U = 1
    WALL_R = 2
    WALL_D = 4
    WALL_L = 8
    WALL_S = 16
    WALL_Z = 32
    WALL_ANY = WALL_U | WALL_R | WALL_D | WALL_L | WALL_S | WALL_Z

def make_iso_tm()
    var izoTmY, izoTmX : float3x3
    let a = asin(tan(PI * 30.0 / 180.0))
    let b = PI * 45.0/ 180.0
    let cosa = cos(a)
    let sina = sin(a)
    let cosb = cos(b)
    let sinb = sin(b)

    izoTmX[0] = float3(1., 0., 0.)
    izoTmX[1] = float3(0., cosa, -sina)
    izoTmX[2] = float3(0.,sina, cosa)

    izoTmY[0] = float3(cosb, 0., sinb)
    izoTmY[1] = float3(0., 1., 0.)
    izoTmY[2] = float3(-sinb, 0., cosb)
    var res= izoTmX * izoTmY
    res[1] = float3(0,0,1)
    return res

def make_inv_tm(tm)
    var res : float3x4
    res[0] = tm[0]
    res[1] = tm[1]
    res[2] = tm[2]
    return float3x3(inverse(res))

let
    camera_tm : float3x3 = make_iso_tm()
    camera_inv : float3x3 = make_inv_tm(camera_tm)
var
    camera_pos : float2

def camera_without_translate(pos : float2)
    let p = pos * float(CELL_SIZE)
    return (camera_tm * float3(p.x, 0.0, p.y)).xy

def camera(pos : float2)
    return camera_without_translate(pos) + camera_pos

def uncamera(pos : float2)
    return (camera_inv * float3(pos.x - camera_pos.x, pos.y - camera_pos.y, 0.0)).xz / float(CELL_SIZE)

///////////// INIT

def read_entities(var obj; var mapping : table<string;int>)
    if key_exists(obj, "entities")
        var templates& = obj["entities"].value as _array
        for entry in templates
            var components& = entry as _object
            create_entity <| @ [[:=components,:=mapping]]( eid, cmp )
                cmp |> set("eid", eid)
                for field in components |> keys()
                    let fieldSplit <- split(field, ":")
                    let valRaw& = components[field].value
                    let compName = fieldSplit[0]
                    let compType = fieldSplit[1]
                    if compType == "b"
                        cmp |> set(compName, valRaw as _bool)
                    if compType == "r"
                        cmp |> set(compName, float(valRaw as _number))
                    if compType == "i"
                        cmp |> set(compName, int(valRaw as _number))
                    if compType == "eid"
                        cmp |> set(compName, INVALID_ENTITY_ID)
                    if compType == "c"
                        let valRawArr& = valRaw as _array
                        let r = float(valRawArr[0].value as _number)
                        let g = float(valRawArr[1].value as _number)
                        let b = float(valRawArr[2].value as _number)
                        cmp |> set(compName, make_color(r,g,b))
                    if compType == "k"
                        let val = mapping |> find(valRaw as _string)
                        if val != null
                            cmp |> set(compName, int(*val))
                    if compType == "p2"
                        let valRawArr& = valRaw as _array
                        cmp |> set(compName, float2(float(valRawArr[0].value as _number), float(valRawArr[1].value as _number)))
                    if compType == "i2"
                        let valRawArr& = valRaw as _array
                        cmp |> set(compName, int2(int(valRawArr[0].value as _number), int(valRawArr[1].value as _number)))

def read_walls(var obj)
    if !key_exists(obj, "level_length") ||  !key_exists(obj, "ship")
        return
    let levelLength = int(obj["level_length"] as _number)
    if levelLength <= 0
        return
    var arr & = obj["ship"].value as _array
    for index, elem in range(0, length(arr)), arr
        let walls = int(elem as _number)
        let y = index / levelLength
        let x = index % levelLength
        create_entity <| @ ( eid, cmp )
            cmp |> set("eid", eid)
            cmp |> set("walls", walls)
            cmp |> set("cell", int2(x,y))

def load_key_mapping()
    var mapping : table<string;int>
    fopen("./keymap.json", "rb") <| $(file : FILE const? const)
        fread(file) <| $(fileContent : string const#)
            var error : string
            var data = read_json(fileContent, error)
            if data == null || length(error) > 0 || !(data is _object)
                return
            var entries& = data as _object
            for key in entries |> keys()
                if entries[key] is _number
                    mapping[key] = int(entries[key] as _number)
    return <- mapping

def load_level()
    var mapping <- load_key_mapping()
    fopen("./level.json", "rb") <| $(file : FILE const? const)
        fread(file) <| $(fileContent : string const#)
            var error : string
            var data = read_json(fileContent, error)
            if data == null || length(error) > 0 || !(data is _object)
                return
            var obj& = data as _object
            if error != ""
                return
            read_walls(obj)
            read_entities(obj, mapping)
            // read_interactable(obj)

def load_text_buffer()
    fopen("./strings.json", "rb") <| $(file : FILE const? const)
        fread(file) <| $(fileContent : string const#)
            var error : string
            var data = read_json(fileContent, error)
            if data == null || length(error) > 0 || !(data is _array)
                return
            text_buffer |> clear()
            for entry in data as _array
                text_buffer |> push(entry.value as _string)

[export]
def initialize(is_hard_reload: bool)
    set_window_title("deSolation protocol")
    walls_img <- create_managed_image("img/wall_atlas.png")
    hero_img <- create_managed_image("img/solitude_atlas.png")
    interact_img <- create_managed_image("img/interact_atlas.png")
    hero_img |> make_image_color_transparent(0xFF640064)
    interact_img |> make_image_color_transparent(0xFF640064)
    walls_img |> make_image_color_transparent(0xFF640064)

    load_text_buffer()
    if is_hard_reload
        load_level()
        commit()


///////////// ACT

[decs(stage=update, REQUIRE=input)]
def input_move(var inputDir : float2&)
    let isUp = get_key(VK_UP) || get_key(VK_W)
    let isDown = get_key(VK_DOWN) || get_key(VK_S)
    let isLeft = get_key(VK_LEFT)  || get_key(VK_A)
    let isRight = get_key(VK_RIGHT)  || get_key(VK_D)
    let inputY =  isUp ? float2(-1., -1.) : isDown ? float2(1., 1.) : float2(0.)
    let inputX =  isLeft ? float2(-1., 1.) : isRight ? float2(1., -1.) : float2(0.)
    inputDir = normalize(inputX + inputY)

[decs(stage=update, REQUIRE=input)]
def move_set_dir(inputDir : float2; var dir : float2&)
    if length_sq(inputDir) > 0.
        dir = inputDir

[decs(stage=update)]
def gravity_set_can_walk(pos : float2; moveSpeed : float2; var canWalk : bool)
    let cellWalls = get_walls(int2(pos))
    let hasFloor = cellWalls >= 0
    if !hasFloor
        canWalk = false
        return
    canWalk = true
    if length_sq(moveSpeed) <= 0.0
        return
    query() <| $ [REQUIRE(gravitation)](state : bool)
        if !state
            let hasContactWithWalls = (((cellWalls & WALL_ANY) != 0) ||
                ((get_walls(int2(pos)+int2(1,0)) & (WALL_L|WALL_U|WALL_D|WALL_S|WALL_Z)) != 0) ||
                ((get_walls(int2(pos)+int2(-1,0)) & (WALL_R|WALL_U|WALL_D|WALL_S|WALL_Z)) != 0) ||
                ((get_walls(int2(pos)+int2(0,1)) & (WALL_U|WALL_L|WALL_R|WALL_S|WALL_Z)) != 0) ||
                ((get_walls(int2(pos)+int2(0,-1)) & (WALL_D|WALL_L|WALL_R|WALL_S|WALL_Z)) != 0))
            canWalk = hasContactWithWalls

[decs(stage=update, REQUIRE=input)]
def move_set_speed(inputDir : float2; walkSpeed : float; canWalk: bool; var moveSpeed : float2)
    if canWalk
        moveSpeed = inputDir * walkSpeed * get_delta_time()

[decs(stage=update, before=update_move)]
def set_wish_pos(moveSpeed : float2; inputDir : float2; var wishPos : float2&; pos : float2)
    wishPos = pos + moveSpeed
    let wishCell = float2(int(wishPos.x), int(wishPos.y))
    wishPos = clamp(wishPos, wishCell + float2(0.01, 0.01), wishCell + float2(0.99, 0.99))

def line_line_intersect(p1, p2, p3, p4 : float2; var mua : float&; var mub : float&)
    let p13 = p1 - p3
    let p43 = p4 - p3
    let p21 = p2 - p1
    let p43lenSq = length_sq(p43)

    let denom = length_sq(p21) * p43lenSq - dot(p43, p21) * dot(p43, p21)
    let numer = dot(p13, p43) * dot(p43, p21) - dot(p13, p21) * p43lenSq

    mua = denom > 0.0 ? numer / denom : denom
    mub = p43lenSq > 0.0 ? ( dot(p13, p43) + dot(p43, p21) * mua ) / p43lenSq : p43lenSq


def cross(a, b : float2)
    return a.x*b.y - a.y*b.x

def get_hit_pos(from, to, corner_left, corner_right : float2)
    var mua, mub : float
    line_line_intersect(from, to, corner_left, corner_right, mua, mub)
    if mub >= 0. && mub <= 1. && mua >= 0. && mua <= 1.
        return from + (to-from) * mua * 0.8
    return to

def is_segments_intersect(from, to, segment_a, segment_b : float2)
    var mua, mub : float
    line_line_intersect(from, to, segment_a, segment_b, mua, mub)
    let res = mub >= 0. && mub <= 1. && mua > 0. && mua <= 1.
    return res

def check_collision(from, to : float2; cell : int2; walls : int)
    let cellPos = float2(cell.x, cell.y)
    if (walls & WALL_U) != 0 && is_segments_intersect(from, to, cellPos + float2(0., 0.), cellPos + float2(1., 0.))
        return false
    if (walls & WALL_D) != 0 && is_segments_intersect(from, to, cellPos + float2(0.,1.), cellPos + float2(1.,1.))
        return false
    if (walls & WALL_L) != 0 && is_segments_intersect(from, to, cellPos + float2(0.,1.), cellPos + float2(0.,0.))
        return false
    if (walls & WALL_R) != 0 && is_segments_intersect(from, to, cellPos + float2(1.,0.), cellPos + float2(1.,1.))
        return false
    if (walls & WALL_S) != 0 && is_segments_intersect(from, to, cellPos + float2(0.,0.), cellPos + float2(1.,1.))
        return false
    if (walls & WALL_Z) != 0 && is_segments_intersect(from, to, cellPos + float2(1., 0.), cellPos + float2(0.,1.))
        return false
    return true

def get_cell_index(cell : int2; level_length : int)
    return cell.y * level_length + cell.x

def get_walls(to_cell : int2)
    var res = -1
    find_query() <| $(cell : int2; walls : int)
        if cell == to_cell
            res = walls
            return true
        return false
    return res

def set_wall_status(to_cell : int2; wall : int)
    find_query() <| $ (cell : int2; var walls : int&)
        if cell == to_cell
            walls |= wall
            return true
        return false

def reset_wall_status(to_cell : int2; wall : int)
    find_query() <| $ (cell : int2; var walls : int&)
        if cell == to_cell
            walls &= ~wall
            return true
        return false

def closed_range(var start, end : int) : iterator<int>
    let step = sign(end - start)
    var count = abs(end - start) + 1
    return <- each() <| @(var result:int&) : bool
        result = start
        start += step
        --count
        return count >= 0

def collect_cells(from, to : float2)
    var cellList : array<int2>
    let fromCell = int2(int(from.x), int(from.y))
    let toCell = int2(int(to.x), int(to.y))
    let dir = to - from
    if dir.x == 0.
        let cx = int(from.x)
        for cy in closed_range(fromCell.y, toCell.y)
            cellList |> push(int2(cx, cy))
        return <- cellList

    let lineK = dir.y / dir.x
    let minCellY = min(fromCell.y, toCell.y)
    let maxCellY = max(fromCell.y, toCell.y)
    for cx in closed_range(fromCell.x, toCell.x)
        var y1 = clamp(int(from.y + lineK * (float(cx) - from.x)), minCellY, maxCellY)
        var y2 = clamp(int(from.y + lineK * (float(cx+1) - from.x)), minCellY, maxCellY)
        for cy in closed_range(y1, y2)
            cellList |> push(int2(cx, cy))
    return <- cellList


[decs(stage=update, after=input_move, before=update_move)]
def move_check_collision(pos : float2; var wishPos : float2&)
    if length_sq(pos - wishPos) <= 0.0
        return
    let dirNorm = normalize(wishPos-pos)
    let cells <- collect_cells(pos, wishPos + dirNorm * 0.5)
    for cell in cells
        let cellWalls = get_walls(cell)
        if cellWalls < 0 || !check_collision(pos, wishPos, cell, cellWalls)
            wishPos = pos


[decs(stage=update)]
def update_move(wishPos : float2; var pos : float2&; var curWalkSpeed : float2&)
    curWalkSpeed = wishPos - pos
    pos = wishPos
    pos.x = clamp(pos.x, floor(pos.x)+0.01, ceil(pos.x)-0.01)
    pos.y = clamp(pos.y, floor(pos.y)+0.01, ceil(pos.y)-0.01)

[decs(stage=update)]
def update_oxygen_depletion(oxygenDepletionSpeedMin, oxygenDepletionSpeedMax : float; var oxygenDepletionSpeed : float&)
    query() <| $[REQUIRE(oxygenGeneration)] (state : bool)
        oxygenDepletionSpeed = state ? oxygenDepletionSpeedMin : oxygenDepletionSpeedMax

[decs(stage=update)]
def update_oxygen(oxygenDepletionSpeed : float; var oxygenLevel : float&)
    let dt = get_delta_time()
    oxygenLevel = clamp(oxygenLevel - oxygenDepletionSpeed * dt, 0.,1.)

[decs(stage=update)]
def check_doors(state : bool; wallPos : int2; wallType : int)
    if state
        set_wall_status(wallPos, wallType)
    else
        reset_wall_status(wallPos, wallType)

[decs(stage=update, REQUIRE=interactable)]
def update_interactable_button_pos(pos : float2; var buttonPos : float2&)
    buttonPos = camera(pos + float2(1,1))

[decs(stage=update, REQUIRE=input)]
def check_interact_dist(pos : float2; maxUseDist : float)
    let maxUseDistSq = maxUseDist * maxUseDist
    let heroPos = pos
    query() <| $[REQUIRE(interactable)] (pos : float2; var active : bool&)
        active = length_sq(pos - heroPos) < maxUseDistSq

[decs(stage=update)]
def check_menu_disabled(active : bool; var menuSelected : bool&)
    if !active
        menuSelected = false

[decs(stage=update)]
def check_button_under_mouse(active : bool; buttonPos : float2; buttonRectMin : float2; buttonRectMax : float2; var isUnderMouse : bool&)
    if !active
        return
    let mouseRelativePos = get_mouse_position() - buttonPos
    isUnderMouse = (mouseRelativePos.x > buttonRectMin[0] && mouseRelativePos.y > buttonRectMin[1]
                 && mouseRelativePos.x < buttonRectMax[0] && mouseRelativePos.y < buttonRectMax[1])

def input_move_camera()
    let mousePos = get_mouse_position()
    let screenSize = float2(get_screen_width(), get_screen_height())
    let mouseRelative = mousePos - screenSize * 0.5
    let middle = screenSize * 0.5
    query() <| $[REQUIRE(input)] (pos : float2)
        camera_pos = float2(int2(middle -camera_without_translate(pos) - mouseRelative * 0.2))

let
    SHAKE_MAGNITUDE = 30.0
    SHAKE_SPEED = 0.15
    SHAKE_OCTAVE_AMP_X = [[auto 0.45; 0.40; 0.15]]
    SHAKE_OCTAVE_AMP_Y = [[auto 0.55; 0.35; 0.10]]
    SHAKE_OCTAVE_PHY_X = [[auto 0.45; 0.38; 0.86]]
    SHAKE_OCTAVE_PHY_Y = [[auto 0.95; 0.81; 0.67]]

def wave(t,speed : float; amp, phi : float[3])
    return amp[0] * sin(phi[0]+speed*t) + amp[1] * sin(phi[1]+speed*t*4.0) + amp[2] * sin(phi[2]+speed*t*8.0)

def shake(t : float)
    return float2(
        wave(t, SHAKE_SPEED, SHAKE_OCTAVE_AMP_X, SHAKE_OCTAVE_PHY_X),
        wave(t, SHAKE_SPEED, SHAKE_OCTAVE_AMP_Y, SHAKE_OCTAVE_PHY_Y)
    ) * SHAKE_MAGNITUDE

def shake_camera_effect()
    let t = get_time_after_start()
    let a,b,c,d : float

    camera_pos += float2(int2(shake(t)))

[decs(stage=update)]
def bind_to_parent(parent : int; var menu : EntityId&)
    if menu == INVALID_ENTITY_ID
        query() <| $(eid : EntityId; id : int)
            if id == parent
                menu := eid
                // return true
            // return false
    if menu == INVALID_ENTITY_ID
        debug_print("menu not found {parent}")

[decs(stage=update)]
def check_menu_element_status(menu : EntityId; state : bool = true; var active : bool&)
    var elementActive& = active
    elementActive = false
    if !state
        active = false
        return
    query(menu) <| $(menuSelected : bool)
        elementActive = menuSelected

[decs(stage=update)]
def menu_arrange_text(eid : EntityId; menuSelected : bool)
    if !menuSelected
        return
    let textMenuX = get_screen_width() - MENU_WIDTH + MENU_PADDING * 2
    let textMenuY = MENU_PADDING * 2
    let lineOffset = MENU_PADDING + get_current_font_size()
    var textIndex = 0
    query() <| $ (menu : EntityId; state : bool; var textPos : float2&)
        if eid == menu && state
            textPos = float2(textMenuX, textMenuY + lineOffset * textIndex++)

[decs(stage=update)]
def menu_arrange_buttons(eid : EntityId; menuSelected : bool)
    if !menuSelected
        return
    let buttonMenuX = get_screen_width() - MENU_WIDTH + MENU_PADDING * 2
    let buttonMenuY = MENU_BUTTON_SECTION_OFFSET_Y
    let buttonOffset = MENU_BUTTON_HEIGHT + MENU_PADDING
    var buttonIndex = 0
    query() <| $ (menu : EntityId; state : bool; var buttonPos : float2&)
        if eid == menu && state
            buttonPos = float2(buttonMenuX, buttonMenuY + buttonOffset * buttonIndex++)

def enable_menu(selected_menu : EntityId)
    query() <| $(eid : EntityId; var menuSelected : bool&)
        menuSelected = eid == selected_menu

[decs(stage=update)]
def trigger_menu(eid : EntityId; var triggered : bool&)
    if triggered
        enable_menu(eid)
        triggered = false

def action_toggle_state(id : int)
    query() <| $(actionId : int; var state : bool&)
        if actionId == id
            state = !state

def action_trigger(id : int)
    query() <| $ (trigger : int; var triggered : bool&)
        if trigger == id
            triggered = true

def do_action(id : int)
    action_trigger(id)
    action_toggle_state(id)

def mouse_interract()
    let pressed = get_mouse_button_down(MB_LEFT)
    if pressed
        find_query <| $[REQUIRE(interactable)] (eid : EntityId; isUnderMouse : bool)
            if isUnderMouse
                enable_menu(eid)
            return isUnderMouse

        query <| $ (active : bool; triggerAction : int; isUnderMouse : bool)
            if isUnderMouse && active
                do_action(triggerAction)

[export]
def act(dt: float)
    if get_key(VK_ESCAPE)
        schedule_quit_game()
    input_move_camera()
    shake_camera_effect()
    decs_stage("update")
    mouse_interract()


///////////// DRAW


def iso_line(x1, y1, x2, y2 : auto; color : uint)
    let p1 = camera(float2(x1, y1))
    let p2 = camera(float2(x2, y2))
    line(p1.x, p1.y, p2.x, p2.y, color)

def draw_floor(xi, yi)
    let x = xi
    let y = yi
    var posIso = camera((float2(xi+1, yi+1)))
    posIso += float2(-CELL_IMG_SIZE.x * 0.5, - CELL_IMG_SIZE.y)
    posIso.x = floor(posIso.x)
    posIso.y = floor(posIso.y)


    iso_line(x, y, x, y+1, 0x33333333)
    iso_line(x+1, y, x+1, y+1, 0x33333333)
    iso_line(x, y, x+1, y, 0x33333333)
    iso_line(x, y+1, x+1, y+1, 0x33333333)

def draw_wall(xi, yi, wall : int; hero_pos : float2; is_background : bool = true)
    let x = xi * CELL_SIZE
    let y = yi * CELL_SIZE
    var posIso = camera((float2(xi+1, yi+1)))
    posIso += float2(-CELL_IMG_SIZE.x * 0.5, - CELL_IMG_SIZE.y)
    posIso.x = floor(posIso.x)
    posIso.y = floor(posIso.y)

    if (wall & WALL_U) != 0
        let wallIndex = 0.
        let isFront = hero_pos.y < float(yi) && hero_pos.y > float(yi-2) && hero_pos.x <= float(xi+1) && hero_pos.x > float(xi-1)
        let wallRow = isFront ? 1.0 : 0.0
        if isFront != is_background
            walls_img |> draw_image_region(posIso.x, posIso.y, float4(CELL_IMG_SIZE.x * wallIndex, CELL_IMG_SIZE.y * wallRow, CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))
    if (wall & WALL_L) != 0
        let wallIndex = 3.
        let isFront = hero_pos.x < float(xi) && hero_pos.x > float(xi-2) && hero_pos.y > float(yi-1) && hero_pos.y < float(yi+1)
        let wallRow = isFront ? 1.0 : 0.0
        if isFront != is_background
            walls_img |> draw_image_region(posIso.x, posIso.y, float4(CELL_IMG_SIZE.x * wallIndex, CELL_IMG_SIZE.y * wallRow, CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))
    if (wall & WALL_S) != 0
        let wallIndex = 4.
        let relativeHeroCellPos = hero_pos - float2(xi, yi)
        let isFront = relativeHeroCellPos.y < relativeHeroCellPos.x && relativeHeroCellPos.y > relativeHeroCellPos.x - 1. && hero_pos.y > float(yi-1) && hero_pos.x < float(xi+1)
        let wallRow = isFront ? 1.0 : 0.0
        if isFront != is_background
            walls_img |> draw_image_region(posIso.x, posIso.y, float4(CELL_IMG_SIZE.x * wallIndex, CELL_IMG_SIZE.y * wallRow, CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))
    if (wall & WALL_Z) != 0
        let wallIndex = 5.
        let relativeHeroCellPos = hero_pos - float2(xi, yi+1)
        let isFront = relativeHeroCellPos.y < -relativeHeroCellPos.x && relativeHeroCellPos.y > -relativeHeroCellPos.x - 3. && hero_pos.y > float(yi-1) && hero_pos.y <= float(yi+1) && hero_pos.x >= float(xi-1) && hero_pos.x <= float(xi+1)
        let wallRow = isFront ? 1.0 : 0.0
        if isFront != is_background
            walls_img |> draw_image_region(posIso.x, posIso.y, float4(CELL_IMG_SIZE.x * wallIndex, CELL_IMG_SIZE.y * wallRow, CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))
    if (wall & WALL_D) != 0
        let wallIndex = 2.
        let isFront = hero_pos.y <= float(yi+1) && hero_pos.y >= float(yi-1) && hero_pos.x <= float(xi+1) && hero_pos.x > float(xi-1)
        let wallRow = isFront ? 1.0 : 0.0
        if isFront != is_background
            walls_img |> draw_image_region(posIso.x, posIso.y, float4(CELL_IMG_SIZE.x * wallIndex, CELL_IMG_SIZE.y * wallRow, CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))
    if (wall & WALL_R) != 0
        let wallIndex = 1.
        let isFront = hero_pos.x <= float(xi+1) && hero_pos.x >= float(xi-1) && hero_pos.y > float(yi-1) && hero_pos.y < float(yi+1)
        let wallRow = isFront ? 1.0 : 0.0
        if isFront != is_background
            walls_img |> draw_image_region(posIso.x, posIso.y, float4(CELL_IMG_SIZE.x * wallIndex, CELL_IMG_SIZE.y * wallRow, CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))

[decs(stage=draw, before=draw_avatar)]
def draw_level_background(cell : int2; walls : int)
    var heroPos : float2
    query() <| $ [REQUIRE(input)](pos : float2)
        heroPos = pos
    draw_floor(cell.x, cell.y)
    draw_wall(cell.x, cell.y, walls, heroPos, true)

[decs(stage=draw, REQUIRE=avatar)]
def draw_avatar(pos : float2; dir : float2; curWalkSpeed : float2; canWalk : bool)
    let posIso = camera(pos)
    let t = get_time_after_start()
    let isWalkingS = canWalk && abs(curWalkSpeed.y) > 0.0
    let animRow = dir.y >= 0.0 ? 0.0 : 1.0
    let frame = isWalkingS ? (sin(t * 2.0 * PI * ANIM_HERO_WALK_SPEED) > 0.0 ? 1.0 : 2.0) : 0.0
    hero_img |> draw_image_region(posIso.x + HERO_IMG_OFFSET.x, posIso.y + HERO_IMG_OFFSET.y, float4(float(HERO_IMG_SIZE.x) * frame, float(HERO_IMG_SIZE.y) * animRow, float(HERO_IMG_SIZE.x), float(HERO_IMG_SIZE.y)))

[decs(stage=draw, REQUIRE=interactable, after=draw_avatar)]
def draw_interactable(pos : float2; active : bool; isUnderMouse : bool; id: int)
    if !active
        return
    var posIso = camera((pos + float2(1.0, 1.0)))
    posIso.x = floor(posIso.x)
    posIso.y = floor(posIso.y)

    if isUnderMouse
        posIso.y += sin(get_time_after_start() * 10.0) * 1.0
    interact_img |> draw_image_region(posIso.x - CELL_IMG_SIZE.x * 0.5, posIso.y - CELL_IMG_SIZE.y, float4(0., 0., CELL_IMG_SIZE.x, CELL_IMG_SIZE.y))
    text_out(posIso.x, posIso.y, "{id}", 0x000000)


[decs(stage=drawMenu, REQUIRE=button)]
def draw_buttons(active : bool; state : bool; text : int; buttonPos : float2; buttonRectMin : float2; buttonRectMax : float2; isUnderMouse : bool)
    if active && state
        let fontSize = get_current_font_size()
        let buttonWidth = buttonRectMax[0]-buttonRectMin[0]
        let buttonHeight = buttonRectMax[1]-buttonRectMin[1]
        let textOffsetY = buttonHeight *0.5 - float(fontSize)
        fill_rect(buttonPos.x + buttonRectMin[0], buttonPos.y + buttonRectMin[1], buttonWidth, buttonHeight, isUnderMouse ? 0xFF0000 : 0xFFFFFF)
        let textData = uint(text) < uint(length(text_buffer)) ? text_buffer[text] : ""
        text_out(buttonPos.x + buttonRectMin[0] + textOffsetY, buttonPos.y + buttonRectMin[1] + MENU_BUTTON_TEXT_PADING, textData, 0x000000)

[decs(stage=drawMenu)]
def draw_menu_text(active : bool; state : bool; text : int; textPos : float2; color : uint = 0xFFFFFF)
    if active && state
        let fontSize = get_current_font_size()
        let textData = uint(text) < uint(length(text_buffer)) ? text_buffer[text] : ""
        text_out(textPos.x, textPos.y, textData, color)

[decs(stage=draw, after=draw_avatar)]
def draw_level_front(cell : int2; walls : int)
    var heroPos : float2
    query() <| $ [REQUIRE(input)](pos : float2)
        heroPos = pos
    draw_wall(cell.x, cell.y, walls, heroPos, false)

[decs(stage=draw)]
def draw_interactable_menu(menuSelected : bool)
    if !menuSelected
        return
    let menuPosX = get_screen_width() - MENU_WIDTH
    let height = get_screen_height()
    let menuWorkWidth = MENU_WIDTH - 2*MENU_PADDING
    rect(menuPosX, 1, MENU_WIDTH-1, height-2, 0xAAAAAA)
    fill_rect(menuPosX, 1, MENU_WIDTH-1, height-2, 0x333333)
    fill_rect(menuPosX + MENU_PADDING, MENU_PADDING, menuWorkWidth, MENU_TEXT_SECTION_HEIGHT, 0x000000)

[decs(stage=drawMenu, REQUIRE=input)]
def draw_hero_interface(eid : EntityId; oxygenLevel : float; oxygenDepletionSpeed : float; pos : float2; wishPos : float2)
    let height = get_screen_height()
    let menuWorkWidth = MENU_WIDTH - 2*MENU_PADDING
    rect(0, 1, MENU_WIDTH-1, height-2, 0xAAAAAA)
    fill_rect(0, 1, MENU_WIDTH-1, height-2, 0x333333)
    fill_rect(0 + MENU_PADDING, MENU_PADDING, menuWorkWidth, MENU_TEXT_SECTION_HEIGHT, 0x000000)
    let isOxygenCritical = oxygenLevel < 0.3 || oxygenDepletionSpeed > 0.01
    let animX = isOxygenCritical ? int((abs(sin(get_time_after_start()*5.0))) * 5.0) : 0
    text_out(int(animX) + MENU_PADDING*2, MENU_PADDING*2, "O2: {int(oxygenLevel*100.0)}% ", isOxygenCritical ? 0xFF0000 : 0x00B4E0)

def draw_title()
    let t = cvt(10.0 - get_time_after_start(), 0., 5., 0., 1.0)

    enable_alpha_blend()
    text_out(400, 300, "deSolation", make_color(1.,1.,1.,t))
    text_out(450, 320, "protocol", make_color(1.,1.,1.,t))
    disable_alpha_blend()

def draw_debug()
    for i, entry in iter_range(debug_text_buffer), debug_text_buffer
        text_out(0, 16 * i, entry._0, 0xFFFFFF)
    let size = length(debug_text_buffer)
    let time = get_time_after_start()
    for i in iter_range(debug_text_buffer)
        let index = size-1-i
        if time - debug_text_buffer[index]._1 > DEBUG_TEXT_TIME
            erase(debug_text_buffer, index)


[export]
def draw()
    enable_alpha_blend()
    decs_stage("draw")
    disable_alpha_blend()
    draw_interactable_menu()
    set_font_size(14)
    decs_stage("drawMenu")
    draw_title()
    draw_debug()