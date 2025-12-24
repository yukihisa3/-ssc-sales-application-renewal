# NJH9210B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NJH9210B.COB`

## ソースコード

```cobol
******************************************************************
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： ＥＤＩＣシステム（共通）
*   　モジュール名       ： ＥＤＩＣエンチョー受領データ変換
*   　作成日／更新日     ： 2020/06/09
*   　作成日／更新者     ：
*   　処理概要           ： ＥＤＩＣ受領データを読み、
*                           ＥＤＩＣ受領ＭＳＧファイルを作
*                           成する。
*     更新日／更新者     ：
*     更新日／更新者     ：
*     修正概要           ：
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            NJH9210B.
*                 流用：NJH9110M.TOKSLIBS(マキバ）
******************************************************************
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/08/19.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*ＥＤＩＣ受領データ（受信）
     SELECT   CVCSG001  ASSIGN    TO   DA-01-S-CVCSG001
                        ACCESS    MODE SEQUENTIAL
                        FILE STATUS    IS   RCV-ST.
*ＥＤＩＣ受領ＭＳＧファイル（キー１）
     SELECT   EDJYURF   ASSIGN         DA-01-VI-EDJYURL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYR-F011
                                       JYR-F012
                                       JYR-F013
                                       JYR-F02
                                       JYR-F03
                                       JYR-F04
                                       JYR-F05
                        FILE STATUS    IS   JYR-ST.
*ＥＤＩＣ受領ＭＳＧファイル（キー２）
     SELECT   EDJYURL2  ASSIGN         DA-01-VI-EDJYURL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JY2-F013
                        FILE STATUS    IS   JY2-ST.
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-ST.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-ST.
*
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*ＥＤＩＣ受領データ（受信）
******************************************************************
 FD  CVCSG001           BLOCK     CONTAINS  1   RECORDS
                        LABEL     RECORD   IS   STANDARD.
 01  CVCSG001-REC.
     03  CVCSG001-F01             PIC   X(004).
     03  CVCSG001-FIL             PIC   X(380).
******************************************************************
*ＥＤＩＣ受領ＭＳＧファイル（キー１）
******************************************************************
 FD  EDJYURF
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDJYURF   OF        XFDLIB
              JOINING   JYR       PREFIX.
******************************************************************
*ＥＤＩＣ受領ＭＳＧファイル（キー２）
******************************************************************
 FD  EDJYURL2
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDJYURF   OF        XFDLIB
              JOINING   JY2       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                      PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                      PIC  9(08)     VALUE  ZERO.
 01  SHOTBL1-INV-FLG              PIC  X(03)     VALUE   SPACE.
 01  MEIMS1-INV-FLG               PIC  X(03)     VALUE   SPACE.
*
     COPY     EDJYURF   OF        XFDLIB
              JOINING   WJYR      PREFIX.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  RCV-ST                   PIC  X(02).
     03  JYR-ST                   PIC  X(02).
     03  JY2-ST                   PIC  X(02).
     03  TBL-ST                   PIC  X(02).
     03  MEI-ST                   PIC  X(02).
*バッチ
*****  システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC  9(06)     VALUE  ZERO.
     03  SYS-DATEW                PIC  9(08)     VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC  9(04).
         05  SYS-MM               PIC  9(02).
         05  SYS-DD               PIC  9(02).
*****  システム時刻ワーク
 01  SYS-TIME                     PIC  9(08).
 01  FILLER                       REDEFINES      SYS-TIME.
     03  SYS-HHMMSS               PIC  9(06).
     03  SYS-MS                   PIC  9(02).
****  文字変換
****    店舗ＣＤ
 01  WK-WJYR-F212                 PIC  X(13).
 01  FILLER                       REDEFINES      WK-WJYR-F212.
     03  HK-WJYR-F212             PIC  9(13).
****    検収日
 01  WK-WJYR-F230                 PIC  X(08).
 01  FILLER                       REDEFINES      WK-WJYR-F230.
     03  HK-WJYR-F230             PIC  9(08).
****    伝票番号
 01  WK-WJYR-F208                 PIC  X(10).
 01  FILLER                       REDEFINES      WK-WJYR-F208.
     03  HK-WJYR-F208             PIC  9(10).
 01  BRK-WJYR-F208                PIC  9(10)   VALUE  ZERO.
 01  WK-GYO                       PIC  9(02)   VALUE  ZERO.
****    行番号
 01  WK-WJYR-F302                 PIC  X(04).
 01  FILLER                       REDEFINES      WK-WJYR-F302.
     03  HK-WJYR-F302             PIC  9(04).
***  セクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(05)     VALUE " *** ".
     03  S-NAME                   PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  RCV-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ受領データ（受信）エラー".
     03  JYR-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ受領ＭＳＧ（キー１）エラー".
     03  JY2-ERR                  PIC  N(20)     VALUE
         NC"ＥＤＩＣ受領ＭＳＧ（キー２）エラー".
     03  TBL-ERR                  PIC  N(20)     VALUE
         NC"商品変換ＴＢＬ（キー１）エラー".
     03  MEI-ERR                  PIC  N(20)     VALUE
         NC"商品名称マスタ（キー１）エラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9210B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9210B".
         05  FILLER               PIC  X(11)     VALUE
                                        " END   *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN              PIC  X(01).
     03  LINK-IN-YMD6             PIC  9(06).
     03  LINK-IN-YMD8             PIC  9(08).
     03  LINK-OUT-RET             PIC  X(01).
     03  LINK-OUT-YMD8            PIC  9(08).
*パラメタ定義
 LINKAGE                SECTION.
* 入力パラメタ
 01  PARA-AREA.
     03  PARA-IN-JUSIN-HI         PIC  9(08).
     03  PARA-IN-JUSIN-JI         PIC  9(04).
     03  PARA-IN-JUSIN-TOR        PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-AREA.
 DECLARATIVES.
 RCV-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  CVCSG001.
     DISPLAY       RCV-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       RCV-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 JYR-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDJYURF.
     DISPLAY       JYR-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JYR-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 JY2-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  EDJYURL2.
     DISPLAY       JY2-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       JY2-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 TBL-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  SHOTBL1.
     DISPLAY       TBL-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       TBL-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 MEI-ERR                    SECTION.
     USE      AFTER         EXCEPTION PROCEDURE  MEIMS1.
     DISPLAY       MEI-ERR  UPON      CONS.
     DISPLAY       SEC-NAME UPON      CONS.
     DISPLAY       MEI-ST   UPON      CONS.
     MOVE          "4000"   TO        PROGRAM-STATUS.
     STOP          RUN.
 END       DECLARATIVES.
******************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
*    DISPLAY "GENERAL-PROCESS" UPON CONS.
     MOVE     "PROCESS-START"      TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =    "END".
     PERFORM  END-SEC.
*
******************************************************************
*             初期処理                                         *
******************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"           TO   S-NAME.
*ＥＤＩＣ受領ＭＳＧ内取引先ＣＤ削除
     PERFORM EDJYURL2-DEL-SEC.
*
     OPEN     INPUT     CVCSG001  SHOTBL1  MEIMS1.
     OPEN     I-O       EDJYURF.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE      ZERO           TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
*ＥＤＩＣ受領データ読込
     PERFORM  CVCSG001-READ-SEC.
*
 INIT-EXIT.
     EXIT.
******************************************************************
*              メイン処理                                      *
******************************************************************
 MAIN-SEC     SECTION.
*
     MOVE     "MAIN-SEC"          TO   S-NAME.
*レコード種別にてり振分
     EVALUATE  CVCSG001-F01
         WHEN  "3000"
                INITIALIZE                 WJYR-REC
                MOVE  CVCSG001-REC     TO  WJYR-F100
         WHEN  "3001"
                INITIALIZE                 WJYR-F200
                MOVE  CVCSG001-REC     TO  WJYR-F200
         WHEN  "3002"
                INITIALIZE                 WJYR-F300
****************MOVE  CVCSG001-REC     TO  WJYR-F300
                MOVE  CVCSG001-REC(1:65)   TO  WJYR-F300(1:65)
                MOVE  CVCSG001-REC(66:24)  TO  WJYR-F300(67:24)
                MOVE  CVCSG001-REC(91:157) TO  WJYR-F300(93:157)
****************ファイル出力する準備を行う
****************初期化
                MOVE  SPACE            TO  JYR-REC
                INITIALIZE                 JYR-REC
                MOVE  WJYR-F100        TO  JYR-F100
                MOVE  WJYR-F200        TO  JYR-F200
                MOVE  WJYR-F300        TO  JYR-F300
                MOVE  PARA-IN-JUSIN-HI TO  JYR-F011
                MOVE  PARA-IN-JUSIN-JI TO  JYR-F012
                MOVE  PARA-IN-JUSIN-TOR TO JYR-F013
                MOVE  WJYR-F212        TO  WK-WJYR-F212
                MOVE  HK-WJYR-F212     TO  JYR-F02
                MOVE  WJYR-F230        TO  WK-WJYR-F230
                MOVE  HK-WJYR-F230     TO  JYR-F03
                MOVE  WJYR-F208        TO  WK-WJYR-F208
                MOVE  HK-WJYR-F208     TO  JYR-F04
****************DISPLAY "WJYR-F302 = " WJYR-F302  UPON CONS
                IF  BRK-WJYR-F208 NOT = HK-WJYR-F208
                    MOVE  ZERO         TO  WK-GYO
                    MOVE  HK-WJYR-F208 TO  BRK-WJYR-F208
                END-IF
                ADD   1                TO  WK-GYO
                MOVE  WK-GYO           TO  JYR-F05
****************商品名取得（カナ）
                IF  JYR-F311 = SPACE
                    MOVE  PARA-IN-JUSIN-TOR TO TBL-F01
                    MOVE  JYR-F307(2:13)    TO TBL-F02
*********DISPLAY "TBL-F01 02 = "TBL-F01 " - " TBL-F02 UPON CONS
                    PERFORM SHOTBL1-READ-SEC
*********DISPLAY "SHOTBL1-INV-FLG = " SHOTBL1-INV-FLG UPON CONS
                    IF  SHOTBL1-INV-FLG = "INV"
                        MOVE ALL "*"        TO JYR-F311
                    ELSE
*******DISPLAY "TBL-F03132 = "TBL-F031 " - " TBL-F032 UPON CONS
                        MOVE TBL-F031       TO MEI-F011
                        MOVE TBL-F032       TO MEI-F012
                        PERFORM  MEIMS1-READ-SEC
*********DISPLAY "MEIMS1-INV-FLG = " MEIMS1-INV-FLG UPON CONS
                        IF  MEIMS1-INV-FLG = "INV"
                            MOVE ALL "*"    TO JYR-F311
                        ELSE
                            MOVE MEI-F031   TO JYR-F311
                            MOVE MEI-F032   TO JYR-F312
                        END-IF
                    END-IF
                END-IF
                COMPUTE JYR-F316 =
                        ( JYR-F315  *  JYR-F325 )  /  10
                WRITE JYR-REC
                ADD   1                TO  WRT-CNT
     END-EVALUATE.
*
*ＥＤＩＣ受領データ読込
     PERFORM  CVCSG001-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
******************************************************************
*              終了処理                                        *
******************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"           TO   S-NAME.
*件数表示
     DISPLAY NC"初回削除　件数"  " = " DEL-CNT  UPON CONS.
     DISPLAY NC"受信Ｆ　　件数"  " = " RD-CNT   UPON CONS.
     DISPLAY NC"受領ＭＳＧ作成"  " = " WRT-CNT  UPON CONS.
*ファイルのクローズ
     CLOSE     CVCSG001  EDJYURF  SHOTBL1 MEIMS1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ受領データ読込
******************************************************************
 CVCSG001-READ-SEC      SECTION.
     MOVE    "CVCSG001-READ-SEC"          TO   S-NAME.
*ファイル読込
     READ  CVCSG001
           AT       END MOVE  "END"       TO   END-FLG
                        GO                TO   CVCSG001-READ-EXIT
     END-READ.
*
     ADD            1                     TO   RD-CNT.
     IF  RD-CNT(6:3) = "000" OR "500"
         DISPLAY "## NJH9210B RD-CNT = " RD-CNT  UPON CONS
     END-IF.
*
 CVCSG001-READ-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ受領ＭＳＧファイル削除処理（スタート）
******************************************************************
 EDJYURL2-DEL-SEC       SECTION.
     MOVE    "EDJYURF-DEL-SEC"            TO   S-NAME.
*ファイルをＯＰＥＮする。
     OPEN  I-O  EDJYURL2.
*スタートをかける
     MOVE     SPACE                       TO   JY2-REC.
     INITIALIZE                                JY2-REC.
     MOVE     PARA-IN-JUSIN-TOR           TO   JY2-F013.
     START  EDJYURL2  KEY  IS  >=  JY2-F013
            INVALID
            MOVE      "END"               TO   END-FLG
            GO                            TO   EDJYURL2-DEL-010
     END-START.
*ファイルを読込む
     PERFORM  EDJYURL2-READ-SEC  UNTIL  END-FLG = "END".
 EDJYURL2-DEL-010.
*ファイルをＣＬＯＳＥする。
     CLOSE  EDJYURL2.
*
 EDJYURL2-DEL-EXIT.
     EXIT.
******************************************************************
*ＥＤＩＣ受領ＭＳＧファイル削除処理（削除）
******************************************************************
 EDJYURL2-READ-SEC     SECTION.
     MOVE    "EDJYURL2-READ-SEC"          TO   S-NAME.
*スタートをかける
     READ  EDJYURL2  NEXT  AT  END
                     MOVE  "END"          TO   END-FLG
                     GO                   TO   EDJYURL2-READ-EXIT
     END-READ.
*取引先ＣＤをチェックする
*****DISPLAY "PARA-IN-JUSIN-TOR = " PARA-IN-JUSIN-TOR UPON CONS.
*****DISPLAY "JY2-F013          = " JY2-F013          UPON CONS.
     IF  PARA-IN-JUSIN-TOR  NOT =  JY2-F013
         MOVE  "END"                      TO   END-FLG
         GO                               TO   EDJYURL2-READ-EXIT
     END-IF.
*ファイルを削除する
     DELETE  EDJYURL2.
     ADD     1                            TO   DEL-CNT.
*
 EDJYURL2-READ-EXIT.
     EXIT.
******************************************************************
*商品変換ＴＢＬ読込
******************************************************************
 SHOTBL1-READ-SEC      SECTION.
     MOVE    "SHOTBL1-READ-SEC"           TO   S-NAME.
*
     READ  SHOTBL1
           INVALID       MOVE  "INV"      TO   SHOTBL1-INV-FLG
           NOT  INVALID  MOVE  SPACE      TO   SHOTBL1-INV-FLG
     END-READ.
*
 SHOTBL1-READ-EXIT.
     EXIT.
******************************************************************
*商品名称マスタ読込
******************************************************************
 MEIMS1-READ-SEC       SECTION.
     MOVE    "MEIMS1-READ-SEC"            TO   S-NAME.
*
     READ  MEIMS1
           INVALID       MOVE  "INV"      TO   MEIMS1-INV-FLG
           NOT  INVALID  MOVE  SPACE      TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.

```
