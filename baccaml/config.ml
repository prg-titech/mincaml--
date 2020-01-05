type stack_hybridized =
  [ `True
  | `False
  ]

let sh_flg : stack_hybridized ref = ref `True

type vm_debug =
  [ `True
  | `False
  ]

let vm_debug_flg : vm_debug ref = ref `False
