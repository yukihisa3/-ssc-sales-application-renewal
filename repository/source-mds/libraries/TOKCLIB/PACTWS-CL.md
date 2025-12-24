# PACTWS

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PACTWS.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    PACTWS                             *  ./
/. *   JOB-NAME    :    端末接続                             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PACTWS'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./

    ACTWS WS-WKSTN501   /.FMV6350CL2 WINDOWS98./
    ACTWS WS-WKSTN502   /.FMV6350CL2 WINDOWS98./
    ACTWS WS-SAKATAPC   /.FMV6350DX2 WINDOWS NT WORK-STATION./
    ACTWS WS-WKSTN504
    ACTWS WS-WKSTN505
    ACTWS WS-WKSTN506
    ACTWS WS-WKSTN507
    ACTWS WS-WKSTN508
    ACTWS WS-WKSTN509
    ACTWS WS-WKSTN510
    ACTWS WS-WKSTN511
    ACTWS WS-WKSTN512
    ACTWS WS-WKSTN513
    ACTWS WS-WKSTN514
    ACTWS WS-WKSTN515
    ACTWS WS-WKSTN516
    ACTWS WS-WKSTN517
    ACTWS WS-WKSTN518
    ACTWS WS-WKSTN519
    ACTWS WS-WKSTN520
    ACTWS WS-WKSTN521
    ACTWS WS-WKSTN522
    ACTWS WS-WKSTN523
    ACTWS WS-WKSTN524
    ACTWS WS-WKSTN525
    ACTWS WS-WKSTN526
    ACTWS WS-WKSTN527
    ACTWS WS-WKSTN528
    ACTWS WS-WKSTN529
    ACTWS WS-WKSTNH6A      /.## 片岡配送ｾﾝﾀｰ       ##./
    ACTWS WS-WKSTNH63      /.## ﾌﾊﾞｻﾐ配送ｾﾝﾀｰ      ##./
    ACTWS WS-WKSTNH60      /.## 大和倉庫配送ｾﾝﾀｰ   ##./
    ACTWS WS-WKSTNH83      /.## 富岡配送ｾﾝﾀｰ       ##./
    ACTWS WS-WKSTNH84      /.## 鴻巣配送ｾﾝﾀｰ       ##./
    ACTWS WS-WKSTNH90      /.## 手綱園芸           ##./
    ACTWS WS-WKSTNH86      /.## 西尾植物１         ##./
    ACTWS WS-WKSTNH8A      /.## 西尾植物２         ##./
    ACTWS WS-WKSTNHE2      /.## 北上端末           ##./
    ACTWS WS-WKSTNH42      /.## 仙台営端末         ##./
    ACTWS WS-WKSTNH45      /.## 北海道支店端末     ##./
    ACTWS WS-WKSTNHT9      /.## 蔦井倉庫端末       ##./
    ACTWS WS-WKSTNH49      /.## 西日本支店端末     ##./
    ACTWS WS-WKSTNHT5      /.## ｶﾄｰﾚｯｸ岡山端末     ##./

    RETURN    PGMEC-@PGMEC


```
