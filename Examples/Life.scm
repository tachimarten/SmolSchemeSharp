#!mjs

# Life.scm

DefineLibrary[
    {Example, Grid},

    Import[{Scheme, Base}, {SmolScheme, Extensions}],
    Export[Make, Rows, Cols, Ref, Each, Rename[Put!, Set!]],

    Begin[
        # Create an NxM grid.
        Make[n, m] := Begin[
            grid := MakeVector[n];
            Do[
                { {i, 0, i + 1} },
                { i == n },
                Local[
                    v := MakeVector[m, False];
                    VectorSet![grid, i, v]
                ]
            ];
            grid
        ];

        Rows[grid] := VectorLength[grid];
        Cols[grid] := VectorLength[VectorRef[grid, 0]];

        Ref[grid, n, m] :=
            -1 < n && n < Rows[grid] && -1 < m && m < Cols[grid] &&
                VectorRef[VectorRef[grid, n], m];

        Put![grid, n, m, v] := VectorSet![VectorRef[grid, n], m, v];

        Each[grid, proc] :=
            Do[
                { {j, 0, j + 1} },
                { j == Rows[grid] },
                Do[
                    { {k, 0, k + 1} },
                    { k == Cols[grid] },
                    proc[j, k, Ref[grid, j, k]]
                ]
            ];
    ]
];

DefineLibrary[
    {Example, Life},

    Import[
        Except[{Scheme, Base}, Set!],
        {Scheme, Write},
        {SmolScheme, Extensions},
        {Example, Grid}
    ],
    Export[Life],

    Begin[
        lifeCount[grid, i, j] := Begin[
            count[i, j] := If[Ref[grid, i, j], 1, 0];

            count[i - 1, j - 1] +
                count[i - 1, j] +
                count[i - 1, j + 1] +
                count[i, j - 1] +
                count[i, j + 1] +
                count[i + 1, j - 1] +
                count[i + 1, j] +
                count[i + 1, j + 1]
        ];

        lifeAlive?[grid, i, j] :=
            Case[lifeCount[grid, i, j],
                { {3}, True },
                { {2}, Ref[grid, i, j] },
                { Else, False },
            ];

        lifePrint[grid] :=
            Each[grid, {i, j, v} => Begin[
                Display[If[v, "*", " "]];
                When[j == Cols[grid] - 1, Newline[]]
            ]];

        Life[grid, iterations] := Do[
            {
                {i, 0, i + 1},
                {grid0, grid, grid1},
                {grid1, Make[Rows[grid], Cols[grid]], grid0}
            },
            { i == iterations },
            Each[grid0, {j, k, v} => Begin[
                a := lifeAlive?[grid0, j, k];
                Set![grid1, j, k, a]
            ]];
            lifePrint[grid1]
        ];
    ];
]

# Main program

Import[
    {Scheme, Base}, {Scheme, Write}, {SmolScheme, Extensions},
    Only[{Example, Life}, Life],
    Rename[Prefix[{Example, Grid}, `Grid-`], { GridMake, MakeGrid }]
];

# Initialize a grid with a glider.
grid := MakeGrid[24, 24];
GridSet![grid, 1, 1, True];
GridSet![grid, 2, 2, True];
GridSet![grid, 3, 0, True];
GridSet![grid, 3, 1, True];
GridSet![grid, 3, 2, True];

# Run for 80 iterations.
Life[grid, 80]
