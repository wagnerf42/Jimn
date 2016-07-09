#!/usr/bin/env python3
"""
this file contains all problematic examples found so for, generating
rounding errors for the kuhn munkres algorithm.
"""
import unittest
from jimn.utils.coordinates_hash import ROUNDER2D
from jimn.point import Point
from jimn.segment import Segment
from jimn.arc import Arc
from jimn.algorithms.sweeping_line_algorithms.kuhn_munkres import kuhn_munkres

RANDOM_RANGE = 30

DANGEROUS_INPUTS = [
    [
        Segment([Point([0.21130667698516703, 0.4123131441764676]),
                 Point([0.8126674687128319, 0.16757138161122065])]),
        Arc(
            0.1562943692585149,
            [
                Point([0.18976017512192975, 0.47458964130790454]),
                Point([0.46016618292550204, 0.4145864838648128])
            ],
            Point([0.3406565633880263, 0.5153107644525876]),
            False
        )
    ],
    [
        Segment([Point([0.175259762235896, 0.501870332127234]),
                 Point([0.3926712752486011, 0.3673897790549907])]),
        Segment([Point([0.025175552314959826, 0.39417403905208437]),
                 Point([0.911534655323246, 0.7376842257794839])]),
        Segment([Point([0.34543252371936606, 0.13156414755780899]),
                 Point([0.01613508456130297, 0.08466318021631503])]),
        Segment([Point([0.6761402929391932, 0.8294443457909856]),
                 Point([0.12120242184273966, 0.38027396545690717])]),
        Segment([Point([0.5012347310891797, 0.22829583433205458]),
                 Point([0.027156584428438046, 0.6862015630890925])])
    ],
    # overlapping part, due to rounding
    [
        Segment([Point([0.01254926675353396, 0.38107148065314167]),
                 Point([0.28285994714323226, 0.7589226075670106])]),
        Segment([Point([0.10239831954130851, 0.5173260340410349]),
                 Point([0.8520355500699942, 0.025211184446686574])]),
        Segment([Point([0.08453496059452359, 0.2733130359072784]),
                 Point([0.3439296062565138, 0.4964105632182719])]),
        Segment([Point([0.40234517737864994, 0.1012803836690066]),
                 Point([0.08038430102096272, 0.7855715758297405])]),
    ],
    # point going back
    [
        Segment([Point([0.02453199106457693, 0.5506792504563169]),
                 Point([0.7543491659367375, 0.6046484534603453])]),
        Segment([Point([0.9932025827397307, 0.981249457716258]),
                 Point([0.04522292132621908, 0.31088054536098564])]),
        Segment([Point([0.15998206210160426, 0.7819098085677472]),
                 Point([0.9322782453144068, 0.22984537753843748])]),
        Segment([Point([0.4375307194623358, 0.9391502996130324]),
                 Point([0.44205567423545744, 0.503454051746504])]),
    ],
    # point rounded towards key
    [
        Segment([Point([0.2985521307629072, 0.7831535608000412]),
                 Point([0.9308861305347477, 0.6853091992380539])]),
        Segment([Point([0.1271332853498992, 0.3387111716111273]),
                 Point([0.47391326263029654, 0.651337712111956])]),
        Segment([Point([0.4137508591353498, 0.26895882846143093]),
                 Point([0.34860994690154046, 0.7471823338397657])]),
        Segment([Point([0.4198890166530219, 0.6704023793418095]),
                 Point([0.31084198068079805, 0.9103151492280626])]),
    ],
    # pb if key is not rounded
    [
        Segment([Point([0.8626698550823712, 0.6521183285051775]),
                 Point([0.20125206705811782, 0.4753846226476536])]),
        Segment([Point([0.5436490553123015, 0.2558525253974886]),
                 Point([0.048586075600635215, 0.6562981466497593])]),
        Segment([Point([0.36367879555935057, 0.3720372068989798]),
                 Point([0.9396528275535977, 0.8721811667668864])]),
        Segment([Point([0.9778211526667054, 0.25277329927264536]),
                 Point([0.1659120452153947, 0.6750229883025565])]),
    ],
    # splitting on current point
    [
        Segment([Point([0.34975820091475307, 0.6841391290047164]),
                 Point([0.8922969479151593, 0.5874775599283555])]),
        Segment([Point([0.9000278475192781, 0.36020238417004047]),
                 Point([0.4471903635801442, 0.8775904115386483])]),
        Segment([Point([0.3691418483447533, 0.6419307318830295]),
                 Point([0.24161792121972547, 0.9196304667141258])]),
    ],
    # almost vertically aligned
    [
        Segment([Point([0.9940140912255592, 0.133975531353974]),
                 Point([0.36671578507210045, 0.3125676955986657])]),
        Segment([Point([0.14545004628901714, 0.5876170970114412]),
                 Point([0.8708192591937208, 0.9214471009603156])]),
        Segment([Point([0.5459463474970254, 0.801258510116124]),
                 Point([0.9389862312908622, 0.03639548931151049])]),
    ]
]


class Test_KuhnMunkres(unittest.TestCase):
    def test1(self):
        for number, input_paths in enumerate(DANGEROUS_INPUTS):
            ROUNDER2D.clear()
            for path in input_paths:
                for point in path.endpoints:
                    ROUNDER2D.hash_point(point)
            print("testing km, test number", number)
            for _ in range(RANDOM_RANGE):
                kuhn_munkres(input_paths)


if __name__ == "__main__":
    unittest.main()
