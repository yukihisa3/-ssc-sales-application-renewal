# SHA0110B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SHA0110B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　発注管理（自動）                  *
*    業務名　　　　　　　：　自動発注数計算                    *
*    モジュール名　　　　：　対象在庫マスタ抽出（マージ）　　　*
*    作成日／更新日　　　：　2000/07/10                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件Ｆを順読みし、自動発注対象の　*
*                            倉庫の在庫データを抽出する。      *
*                            抽出時、商品名称マスタの定番区分  *
*                            ／季節区分を判断し、抽出する。    *
*                            抽出対象は、パラメタにて。        *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0110B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  商品在庫マスタ　  >>---*
     SELECT   ZAMZAIF   ASSIGN    TO        DA-01-VI-ZAMZAIF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   ZAI-F01
                                                 ZAI-F021
                                                 ZAI-F022
                                                 ZAI-F03
                        FILE      STATUS    IS   ZAI-STATUS.
*---<<  商品在庫マスタ実績>>---*
     SELECT   ZAMJISF   ASSIGN    TO        DA-01-VI-ZAMJISF
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   ZAJ-F01
                                                 ZAJ-F02
                                                 ZAJ-F03
                        FILE      STATUS    IS   ZAJ-STATUS.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-HJYOKEN
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*----<< 条件ファイル >>-*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-HMEIMS
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   MEI-F011
                                                 MEI-F012
                        FILE      STATUS    IS   MEI-STATUS.
*---<< 出力ファイル >>-*
     SELECT   OUTF      ASSIGN    TO        DA-01-S-ZAIKOF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   OUT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  商品在庫マスタ 　 >>---*
 FD  ZAMZAIF.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*---<<  商品在庫マスタ実績>>---*
 FD  ZAMJISF.
     COPY     ZAMJISF   OF        XFDLIB
              JOINING   ZAJ       PREFIX.
*----<< 条件ファイル >>-*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 条件ファイル >>-*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<< 出力ファイル >>-*
 FD  OUTF               BLOCK     CONTAINS   4   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   OUT       PREFIX.
************************************************************
*     作業領域                                             *
************************************************************
 WORKING-STORAGE             SECTION.
*在庫データワーク
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZWK       PREFIX.
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  ZAI-STATUS          PIC  X(02).
     02  ZAJ-STATUS          PIC  X(02).
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  OUT-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG            PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG           PIC  X(03)  VALUE SPACE.
 01  CNT-AREA.
     02  READ-CNT            PIC  9(07)  VALUE ZERO.
     02  REWRITE-CNT         PIC  9(07)  VALUE ZERO.
     02  WRITE-CNT           PIC  9(07)  VALUE ZERO.
     02  ERR-CNT             PIC  9(07)  VALUE ZERO.
****  月テーブル添字　****
 01  TBL-AREA.
     02  TBL                 PIC  X(24)     VALUE
                             "080910111201020304050607".
     02  TBL-R               REDEFINES    TBL.
       03  TSUKI             OCCURS    12
                             PIC  9(02).
****  ワ－ク  ***
 01  WK-AREA.
     02  WK-F05              PIC  9(06)     VALUE  ZERO.
     02  WK-F05-R            REDEFINES      WK-F05.
       03  WK-YY             PIC  9(04).
       03  WK-MM             PIC  9(02).
*
 01  IX                      PIC  9(02)  VALUE ZERO.
 01  WK-ZAIF01               PIC  X(02)  VALUE SPACE.
 01  WK-ZAIF01X              PIC  9(01)  VALUE ZERO.
 01  HIT-FLG                 PIC  9(01)  VALUE ZERO.
 01  HJYOKEN-INV-FLG         PIC  X(03)  VALUE SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)  VALUE SPACE.
 01  ZAMJISF-INV-FLG         PIC  X(03)  VALUE SPACE.
 01  OLD-KEY.
     03  OLD-F01             PIC  X(02)  VALUE SPACE.
     03  OLD-F02             PIC  X(16)  VALUE SPACE.
**** 03  OLD-F03             PIC  X(08)  VALUE SPACE.
 01  NEW-KEY.
     03  NEW-F01             PIC  X(02)  VALUE SPACE.
     03  NEW-F02             PIC  X(16)  VALUE SPACE.
*****03  NEW-F03             PIC  X(08)  VALUE SPACE.
**** メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0110B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
 LINKAGE                SECTION.
 01  PARA-TEIBAN            PIC   X(01).
 01  PARA-KISETU1           PIC   X(01).
 01  PARA-KISETU2           PIC   X(01).
 01  PARA-KISETU3           PIC   X(01).
*
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION    USING  PARA-TEIBAN
                                                PARA-KISETU1
                                                PARA-KISETU2
                                                PARA-KISETU3.
**
 DECLARATIVES.
 FILEERR-SEC1                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMZAIF.
     MOVE     "ZAMZAIF"      TO   ERR-FL-ID.
     MOVE     ZAI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC2                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    ZAMJISF.
     MOVE     "ZAMJISF"      TO   ERR-FL-ID.
     MOVE     ZAJ-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HJYOKEN.
     MOVE     "HJYOKEN"      TO   ERR-FL-ID.
     MOVE     JYO-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    HMEIMS.
     MOVE     "HMEIMS "      TO   ERR-FL-ID.
     MOVE     MEI-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5                SECTION.
     USE AFTER       EXCEPTION
                     PROCEDURE    OUTF.
     MOVE     "OUTF  "       TO   ERR-FL-ID.
     MOVE     OUT-STATUS     TO   ERR-STCD.
     DISPLAY  MSG-ABEND1     UPON   CONS.
     DISPLAY  MSG-ABEND2     UPON   CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END     DECLARATIVES.
****************************************************************
 PROCESS-START               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC
                   UNTIL     MAIN-FLG   =   "END".
     PERFORM       END-SEC.
     STOP      RUN.
 PROCESS-EXIT.
     EXIT.
****************************************************************
*      _０　　初期処理                                        *
****************************************************************
 INIT-SEC                    SECTION.
*ファイル読込み
     OPEN    INPUT           ZAMZAIF
                             ZAMJISF
                             HJYOKEN
                             HMEIMS
             OUTPUT          OUTF.
*****　条件ファイル検索(月テーブル）　****
     MOVE    99                  TO   JYO-F01.
     MOVE    "ZAI"               TO   JYO-F02.
     READ    HJYOKEN  INVALID
             MOVE    "END"       TO   END-FLG
             DISPLAY "HJYOKEN INVALID KEY = " JYO-F01 ":" JYO-F02
                      UPON CONS
             GO                  TO   INIT-EXIT
     END-READ.
     MOVE    JYO-F05             TO   WK-F05.
     MOVE    TSUKI(WK-MM)        TO   IX.
*条件Ｆスタート処理
     PERFORM HJYOKEN-START-SEC.
     IF   END-FLG  =  "END"
          GO           TO    INIT-EXIT
     END-IF.
*条件Ｆ読込み
     PERFORM HJYOKEN-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*在庫Ｍスタート処理
     PERFORM ZAMZAIF-START-SEC.
     IF   END-FLG = "END"
          GO           TO    MAIN-90
     END-IF.
 MAIN010.
*在庫Ｍ読込み処理
     PERFORM ZAMZAIF-READ-SEC.
     IF   END-FLG = "END"
          GO           TO    MAIN-90
     END-IF.
 MAIN020.
*条件判定（倉庫がブレイクしたら）
     IF   ZAI-F01 NOT =  JYO-F02
**********MOVE  "END"  TO    END-FLG
          GO           TO    MAIN-90
     ELSE
          ADD     1    TO    READ-CNT
     END-IF.
 MAIN030.
*倉庫コードが変わるまで処理を行う
     MOVE   SPACE      TO    END-FLG.
     PERFORM MAIN-READ UNTIL END-FLG = "END".
 MAIN-90.
*条件Ｆ読込み
     PERFORM HJYOKEN-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                                                              *
****************************************************************
 MAIN-READ                   SECTION.
*    _番毎在庫Ｍ出力
     IF     JYO-F12C   =          ZERO
            PERFORM  SUM1-SEC  UNTIL  END-FLG  =  "END"
     ELSE
*           商品集計出力
            MOVE  ZAI-F01   TO    OLD-F01
            MOVE  ZAI-F02   TO    OLD-F02
            MOVE  OLD-KEY   TO    NEW-KEY
*           在庫ワーク初期化
            MOVE  SPACE      TO   OUT-REC
            INITIALIZE            OUT-REC
*           倉庫＋商品ＣＤ＋品単毎集計
            PERFORM  SUM2-SEC  UNTIL  END-FLG  =  "END"
*           キーの入替え
            MOVE  NEW-F01    TO   OUT-F01
            MOVE  NEW-F02    TO   OUT-F02
*           在庫ワーク初期化
            WRITE                 OUT-REC
            ADD   1          TO   WRITE-CNT
     END-IF.
*
 MAIN-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫＋商品＋品単＋_番                                    *
****************************************************************
 SUM1-SEC                    SECTION.
*在庫レコードセット
     MOVE   ZAI-REC          TO   OUT-REC.
*    実績Ｆ読込み（前月売上数）
     PERFORM ZAMJISF-READ-SEC.
     IF  ZAMJISF-INV-FLG = SPACE
         IF  WK-MM  =  05
             COMPUTE  OUT-F21 = OUT-F21 + ZAJ-F041(IX)
         ELSE
             COMPUTE  OUT-F21 = OUT-F21 + ZAJ-F051(IX)
         END-IF
     END-IF.
     WRITE  OUT-REC.
     ADD    1                TO   WRITE-CNT.
*在庫Ｍ読込み処理
     PERFORM ZAMZAIF-READ-SEC.
*倉庫ＣＤがブレイクしたら
     IF    ZAI-F01 NOT = JYO-F02
           MOVE "END"  TO    END-FLG
     ELSE
           ADD   1     TO    READ-CNT
     END-IF.
*
 SUM1-EXIT.
     EXIT.
****************************************************************
*                                                              *
****************************************************************
 SUM2-SEC                    SECTION.
*キー判定
     IF     NEW-KEY    NOT =      OLD-KEY
            MOVE  OLD-F01    TO   OUT-F01
            MOVE  OLD-F02    TO   OUT-F02
*
            WRITE                 OUT-REC
            ADD   1          TO   WRITE-CNT
*           在庫ワーク（ＷＫ）初期化
            MOVE  SPACE      TO   OUT-REC
            INITIALIZE            OUT-REC
            MOVE  NEW-KEY    TO   OLD-KEY
*
     END-IF.
*    現在庫数
     ADD     ZAI-F04    TO       OUT-F04.
*    前月末在庫数
     ADD     ZAI-F05    TO       OUT-F05.
*    当月入出庫数
     ADD     ZAI-F06    TO       OUT-F06.
*    当月入庫数
     ADD     ZAI-F07    TO       OUT-F07.
*    当月出庫数
     ADD     ZAI-F08    TO       OUT-F08.
*    当月売上数
     ADD     ZAI-F09    TO       OUT-F09.
*    当月返品数
     ADD     ZAI-F10    TO       OUT-F10.
*    次月入庫数
     ADD     ZAI-F11    TO       OUT-F11.
*    次月出庫数
     ADD     ZAI-F12    TO       OUT-F12.
*    次月売上数
     ADD     ZAI-F13    TO       OUT-F13.
*    次月返品数
     ADD     ZAI-F14    TO       OUT-F14.
*    前月入庫数
     ADD     ZAI-F15    TO       OUT-F15.
*    前月出庫数
     ADD     ZAI-F16    TO       OUT-F16.
*    前月（前月末在庫）
     ADD     ZAI-F17    TO       OUT-F17.
*    当月発注数
     ADD     ZAI-F20    TO       OUT-F20.
*    前月売上数
     ADD     ZAI-F21    TO       OUT-F21.
*    未入庫数
     ADD     ZAI-F26    TO       OUT-F26.
*    未出庫数
     ADD     ZAI-F27    TO       OUT-F27.
*    引当済数
     ADD     ZAI-F28    TO       OUT-F28.
*
     PERFORM ZAMJISF-READ-SEC.
     IF  ZAMJISF-INV-FLG = SPACE
         IF  WK-MM  =  05
             COMPUTE  OUT-F21 = OUT-F21 + ZAJ-F041(IX)
         ELSE
             COMPUTE  OUT-F21 = OUT-F21 + ZAJ-F051(IX)
         END-IF
     END-IF.
*在庫Ｍ（実績）読込み
     PERFORM ZAMZAIF-READ-SEC.
*倉庫ＣＤブレイク判定
     IF    ZAI-F01 NOT = JYO-F02
           MOVE "END"        TO   END-FLG
     ELSE
           ADD   1           TO   READ-CNT
           MOVE  ZAI-F01     TO   NEW-F01
           MOVE  ZAI-F02     TO   NEW-F02
     END-IF.
*
 SUM2-EXIT.
     EXIT.
****************************************************************
*      条件Ｆスタート処理                                      *
****************************************************************
 HJYOKEN-START-SEC           SECTION.
*条件ファイル　条件区分＝２０でスタート
     MOVE   20               TO   JYO-F01.
     MOVE   SPACE            TO   JYO-F02.
     START  HJYOKEN  KEY  IS  >=  JYO-F01  JYO-F02
            INVALID
            MOVE   "END"     TO   MAIN-FLG
     END-START.
*
 HJYOKEN-START-EXIT.
     EXIT.
****************************************************************
*      条件Ｆ読込み処理                                        *
****************************************************************
 HJYOKEN-READ-SEC            SECTION.
*条件ファイル倉庫レコード初期ＲＥＡＤ
     READ   HJYOKEN   NEXT
            AT END
            MOVE   "END"     TO   MAIN-FLG
            GO               TO   HJYOKEN-READ-EXIT
     END-READ.
*条件区分＝２０以外の場合
     IF     JYO-F01  NOT =    20
            MOVE   "END"     TO   MAIN-FLG
            GO               TO   HJYOKEN-READ-EXIT
     END-IF.
*条件Ｆ（Ｆ１２Ｂ）が”１”の場合
     IF     JYO-F12B     =    1
            GO               TO   HJYOKEN-READ-SEC
     END-IF.
*フラグクリア
     MOVE   SPACE            TO   END-FLG.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*      在庫Ｍスタート処理                                      *
****************************************************************
 ZAMZAIF-START-SEC           SECTION.
*在庫Ｍスタート
     MOVE  JYO-F02     TO    ZAI-F01.
     MOVE  SPACE       TO    ZAI-F02.
     MOVE  SPACE       TO    ZAI-F03.
     START ZAMZAIF KEY IS >= ZAI-F01 ZAI-F021 ZAI-F022 ZAI-F03
           INVALID    KEY
           MOVE "END"  TO    END-FLG
     END-START.
*
 ZAMZAIF-START-EXIT.
     EXIT.
****************************************************************
*      在庫Ｍスタート処理                                      *
****************************************************************
 ZAMZAIF-READ-SEC            SECTION.
*在庫Ｍ読込み
     READ  ZAMZAIF   AT  END
           MOVE "END"  TO    END-FLG
           GO          TO    ZAMZAIF-READ-EXIT
     END-READ.
*抽出条件判定
*    商品Ｍ存在チェック
     MOVE  ZAI-F021    TO    MEI-F011.
     MOVE  ZAI-F022    TO    MEI-F012.
 ZAM010.
     PERFORM  HMEIMS-READ-SEC.
     IF    HMEIMS-INV-FLG  =  "INV"
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
 ZAM020.
*    自動発注区分チェック
     IF    MEI-F92  NOT =  1
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
 ZAM030.
*    定番区分チェック
     IF    PARA-TEIBAN  NOT =  MEI-F95
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
 ZAM040.
*    季節区分チェック
     IF    PARA-KISETU1 = MEI-F96
     OR    PARA-KISETU2 = MEI-F96
     OR    PARA-KISETU3 = MEI-F96
           CONTINUE
     ELSE
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
*
 ZAMZAIF-READ-EXIT.
     EXIT.
****************************************************************
*      在庫Ｍ読込み処理（実績）                                *
****************************************************************
 ZAMJISF-READ-SEC            SECTION.
*在庫Ｍ（実績）読キーセット
     MOVE  ZAI-F01     TO    ZAJ-F01.
     MOVE  ZAI-F02     TO    ZAJ-F02.
     MOVE  ZAI-F03     TO    ZAJ-F03.
*在庫Ｍ（実績）読込み
     READ  ZAMJISF   INVALID
           MOVE "INV"  TO    ZAMJISF-INV-FLG
           NOT  INVALID
           MOVE SPACE  TO    ZAMJISF-INV-FLG
     END-READ.
*
 ZAMJISF-READ-EXIT.
     EXIT.
****************************************************************
*      商品名称マスタ読込み                                    *
****************************************************************
 HMEIMS-READ-SEC             SECTION.
*商品名称マスタ読込み
     READ  HMEIMS   INVALID
           MOVE "INV"  TO    HMEIMS-INV-FLG
           NOT  INVALID
           MOVE SPACE  TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*      3.0        終了処理                                     *
****************************************************************
 END-SEC                SECTION.
*
     CLOSE              ZAMZAIF
                        HJYOKEN
                        OUTF.
*
     DISPLAY  "*** SHA0110B END   ***"            UPON   CONS.
     DISPLAY  "*** INPUT   = "      READ-CNT      UPON   CONS.
     DISPLAY  "*** OUTPUT  = "      WRITE-CNT     UPON   CONS.
*
 END-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************


```
