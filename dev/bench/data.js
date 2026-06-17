window.BENCHMARK_DATA = {
  "lastUpdate": 1781724515536,
  "repoUrl": "https://github.com/danirana/jsip-exchange",
  "entries": {
    "Order book benchmark": [
      {
        "commit": {
          "author": {
            "email": "86646010+danirana@users.noreply.github.com",
            "name": "Daniella Ranario",
            "username": "danirana"
          },
          "committer": {
            "email": "noreply@github.com",
            "name": "GitHub",
            "username": "web-flow"
          },
          "distinct": true,
          "id": "3a5ceeeb6fa02e7cb6ec53fff2d2a855e46c4781",
          "message": "Merge branch 'jane-street-immersion-program:main' into main",
          "timestamp": "2026-06-17T15:23:48-04:00",
          "tree_id": "b105f708f1d0a3bfac0fc8f703926fc5cb5958f3",
          "url": "https://github.com/danirana/jsip-exchange/commit/3a5ceeeb6fa02e7cb6ec53fff2d2a855e46c4781"
        },
        "date": 1781724515139,
        "tool": "customSmallerIsBetter",
        "benches": [
          {
            "name": "find_match (n=10)",
            "value": 21.591532201243204,
            "unit": "ns"
          },
          {
            "name": "find_match (n=50)",
            "value": 21.884903573301195,
            "unit": "ns"
          },
          {
            "name": "find_match (n=100)",
            "value": 21.38216281263445,
            "unit": "ns"
          },
          {
            "name": "find_match (n=500)",
            "value": 21.56491657994009,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=10)",
            "value": 110.13788522142502,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=50)",
            "value": 501.7499614403727,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=100)",
            "value": 1111.2661355617672,
            "unit": "ns"
          },
          {
            "name": "find_match_miss (n=500)",
            "value": 4869.380107669127,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=10)",
            "value": 197.0000424326988,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=50)",
            "value": 960.749880682051,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=100)",
            "value": 1895.718113270354,
            "unit": "ns"
          },
          {
            "name": "best_bid_offer (n=500)",
            "value": 9187.06279307165,
            "unit": "ns"
          },
          {
            "name": "add+remove (n=100)",
            "value": 1324.0374981482091,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=10)",
            "value": 1116.4601258635382,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=50)",
            "value": 4497.014416388119,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=100)",
            "value": 9347.609012887793,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_cross (n=500)",
            "value": 43515.90480682478,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=10)",
            "value": 582.425196818074,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=50)",
            "value": 2572.839312461638,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=100)",
            "value": 4952.0220743431255,
            "unit": "ns"
          },
          {
            "name": "submit_ioc_miss (n=500)",
            "value": 23677.930261965907,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_10_levels",
            "value": 4918.84173985736,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_50_levels",
            "value": 75805.04008883436,
            "unit": "ns"
          },
          {
            "name": "submit_sweep_100_levels",
            "value": 290312.6152329698,
            "unit": "ns"
          },
          {
            "name": "find_match_alloc (n=100)",
            "value": 21.86619271751065,
            "unit": "ns"
          }
        ]
      }
    ]
  }
}