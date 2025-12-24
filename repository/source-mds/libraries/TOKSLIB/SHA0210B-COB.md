# SHA0210B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SHA0210B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　発注管理（自動）                  *
*    業務名　　　　　　　：　自動発注数計算                    *
*    モジュール名　　　　：　対象在庫マスタ抽出                *
*    作成日／更新日　　　：　2003/06/04                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタより受取った倉庫ＣＤによ　*
*                            り在庫マスタにスタートを掛け、抽  *
*                            出条件に合致した在庫データを在庫  *
*                            ワークに抽出する。                *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0210B.
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
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(03)  VALUE SPACE.
     02  SKIP-FLG            PIC  9(01)  VALUE ZERO.
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
 01  WK-JYO-F12C             PIC  9(01)  VALUE ZERO.
 01  WK-SOKCD                PIC  X(02)  VALUE SPACE.
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
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0210B".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
 LINKAGE                SECTION.
 01  PARA-SOKCD             PIC   X(02).
 01  PARA-TEIBAN            PIC   X(01).
*
*-------------------------------------------------------------*
*             ＭＡＩＮ　　　　ＭＯＤＵＬＥ                    *
*-------------------------------------------------------------*
 PROCEDURE                   DIVISION    USING  PARA-SOKCD
                                                PARA-TEIBAN.
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
                   UNTIL     END-FLG   =   "END".
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
                             HJYOKEN
                             HMEIMS
             OUTPUT          OUTF.
*在庫Ｍスタート処理
     PERFORM ZAMZAIF-START-SEC.
     IF   END-FLG  =  "END"
          GO           TO    INIT-EXIT
     END-IF.
*在庫Ｍ読込み
     PERFORM ZAMZAIF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*      _０　　メイン処理                                      *
****************************************************************
 MAIN-SEC                    SECTION.
*倉庫ＣＤブレイクチェック
     IF  WK-SOKCD  NOT =  ZAI-F01
         PERFORM HJYOKEN-READ-SEC
         IF      HJYOKEN-INV-FLG = "INV"
                 MOVE  1         TO  SKIP-FLG
         ELSE
                 MOVE  ZERO      TO  SKIP-FLG
                 MOVE  JYO-F12C  TO  WK-JYO-F12C
         END-IF
         MOVE    ZAI-F01         TO  WK-SOKCD
     END-IF.
*条件判定
     IF   SKIP-FLG  =  ZERO
          PERFORM  MAIN-READ
     END-IF.
*次在庫マスタ読込み
     PERFORM ZAMZAIF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*                                                              *
****************************************************************
 MAIN-READ                   SECTION.
*    _番毎在庫Ｍ出力
     IF     WK-JYO-F12C   =          ZERO
            PERFORM  SUM1-SEC
     ELSE
*           商品集計出力
            MOVE  ZAI-F01   TO    OLD-F01
            MOVE  ZAI-F02   TO    OLD-F02
            MOVE  OLD-KEY   TO    NEW-KEY
*           在庫ワーク初期化
            MOVE  SPACE      TO   OUT-REC
            INITIALIZE            OUT-REC
*           倉庫＋商品ＣＤ＋品単毎集計
            PERFORM  SUM2-SEC
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
     WRITE  OUT-REC.
     ADD    1                TO   WRITE-CNT.
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
*    発注数量１、２、３集計
     ADD     ZAI-F23    TO       OUT-F23.
     ADD     ZAI-F24    TO       OUT-F24.
     ADD     ZAI-F25    TO       OUT-F25.
*
 SUM2-EXIT.
     EXIT.
****************************************************************
*      条件Ｆ読込み処理                                        *
****************************************************************
 HJYOKEN-READ-SEC            SECTION.
*条件ファイル倉庫レコード
     MOVE   20          TO    JYO-F01.
     MOVE   ZAI-F01     TO    JYO-F02.
     READ   HJYOKEN
            INVALID     MOVE  "INV"   TO  HJYOKEN-INV-FLG
            NOT INVALID MOVE  SPACE   TO  HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*      在庫Ｍスタート処理                                      *
****************************************************************
 ZAMZAIF-START-SEC           SECTION.
*在庫Ｍスタート
     MOVE  PARA-SOKCD  TO    ZAI-F01.
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
 ZAM000.
*    倉庫ＣＤチェック
     IF    PARA-SOKCD  =  SPACE
*          【全倉庫対象】
           CONTINUE
     ELSE
           IF    PARA-SOKCD  <  ZAI-F01
                 MOVE "END"  TO    END-FLG
                 GO          TO    ZAMZAIF-READ-EXIT
           END-IF
     END-IF.
 ZAM010.
*    自動発注ＦＬＧチェック
     IF    ZAI-F97  NOT =  1
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
*抽出条件判定
*    商品Ｍ存在チェック
     MOVE  ZAI-F021    TO    MEI-F011.
     MOVE  ZAI-F022    TO    MEI-F012.
 ZAM020.
     PERFORM  HMEIMS-READ-SEC.
     IF    HMEIMS-INV-FLG  =  "INV"
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
 ZAM030.
*    自動発注区分チェック
     IF    MEI-F92  NOT =  1
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
 ZAM040.
*    定番区分チェック
     IF    PARA-TEIBAN  NOT =  MEI-F95
           GO          TO    ZAMZAIF-READ-SEC
     END-IF.
*
 ZAMZAIF-READ-EXIT.
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
     DISPLAY  "*** SHA0210B END   ***"            UPON   CONS.
     DISPLAY  "*** INPUT   = "      READ-CNT      UPON   CONS.
     DISPLAY  "*** OUTPUT  = "      WRITE-CNT     UPON   CONS.
*
 END-EXIT.
     EXIT.
******************<<  PROGRAM  END  >>**************************


```
