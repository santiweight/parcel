import json as json
import utils as utils
from dataclasses import dataclass
from pathlib import Path
from typing import *
class MaybeInt:
    def __init__(self):
        raise
@dataclass
class NothingInt(MaybeInt):
    pass
    def encode(self):
        return {"tag": "NothingInt"}
@dataclass
class JustInt(MaybeInt):
    myInt : int
    def encode(self):
        return {"tag": "JustInt", "myInt": self.myInt}