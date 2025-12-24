# NJH9111B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/NJH9111B.COB`

## ソースコード

```cobol
******************************************************************
* 　  顧客名             ： (株)サカタのタネ殿
*   　サブシステム名     ： ＨＧ基幹システム　
*   　業務名　　　       ： ＥＤＩＣシステム（共通）
*   　モジュール名       ： ＥＤＩＣ受領データ変換
*   　作成日／更新日     ： 2015/10/26
*   　作成日／更新者     ：
*   　処理概要           ： ＥＤＩＣ受領データを読み、
*                           ＥＤＩＣ受領ＭＳＧファイルを作
*                           成する。（制御バイトなしＶｅｒ）
*     更新日／更新者     ：
*     更新日／更新者     ：
*     修正概要           ：
******************************************************************
 IDENTIFICATION         DIVISION.
******************************************************************
 PROGRAM-ID.            NJH9111B.
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
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                      PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                       PIC  9(08)     VALUE  ZERO.
 01  DEL-CNT                      PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                      PIC  9(08)     VALUE  ZERO.
*
     COPY     EDJYURF   OF        XFDLIB
              JOINING   WJYR      PREFIX.
*プログラムＳＴＡＴＵＳ
 01  WK-ST.
     03  RCV-ST                   PIC  X(02).
     03  JYR-ST                   PIC  X(02).
     03  JY2-ST                   PIC  X(02).
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
 01  WK-WJYR-F212                 PIC  X(05).
 01  FILLER                       REDEFINES      WK-WJYR-F212.
     03  HK-WJYR-F212             PIC  9(05).
****    検収日
 01  WK-WJYR-F230                 PIC  X(08).
 01  FILLER                       REDEFINES      WK-WJYR-F230.
     03  HK-WJYR-F230             PIC  9(08).
****    伝票番号
 01  WK-WJYR-F208                 PIC  X(10).
 01  FILLER                       REDEFINES      WK-WJYR-F208.
     03  HK-WJYR-F208             PIC  9(10).
****    行番号
 01  WK-WJYR-F302                 PIC  X(02).
 01  FILLER                       REDEFINES      WK-WJYR-F302.
     03  HK-WJYR-F302             PIC  9(02).
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
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9111B".
         05  FILLER               PIC  X(11)     VALUE
                                        " START *** ".
     03  MSG-END.
         05  FILLER               PIC  X(05)     VALUE " *** ".
         05  ST-PG                PIC  X(08)     VALUE "NJH9111B".
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
 01  PARA-IN-JUSIN-HI             PIC  9(08).
 01  PARA-IN-JUSIN-JI             PIC  9(04).
 01  PARA-IN-JUSIN-TOR            PIC  9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION USING   PARA-IN-JUSIN-HI
                            PARA-IN-JUSIN-JI
                            PARA-IN-JUSIN-TOR.
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
     OPEN     INPUT     CVCSG001.
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
                MOVE  CVCSG001-REC     TO  WJYR-F300
****************ファイル出力する準備を行う
****************初期化
                MOVE  SPACE            TO  JYR-REC
                INITIALIZE                 JYR-REC
                MOVE  WJYR-F100        TO  JYR-F100
                MOVE  WJYR-F200        TO  JYR-F200
                MOVE  WJYR-F300        TO  JYR-F300
                MOVE  WJYR-F300(1:65)  TO  JYR-F300(1:65)
                MOVE  X"28"            TO  JYR-F300(66:1)
                MOVE  WJYR-F300(66:24) TO  JYR-F300(67:24)
                MOVE  X"29"            TO  JYR-F300(91:1)
                MOVE  SPACE            TO  JYR-F300(92:1)
                MOVE  WJYR-F300(90:295) TO JYR-F300(93:292)
                MOVE  PARA-IN-JUSIN-HI TO  JYR-F011
                MOVE  PARA-IN-JUSIN-JI TO  JYR-F012
                MOVE  PARA-IN-JUSIN-TOR TO JYR-F013
****************店舗ＣＤ
                MOVE  WJYR-F212(9:5)   TO  WK-WJYR-F212
                MOVE  HK-WJYR-F212     TO  JYR-F02
****************最終納品先納品日
                MOVE  WJYR-F230        TO  WK-WJYR-F230
                MOVE  HK-WJYR-F230     TO  JYR-F230
****************取引番号
                MOVE  WJYR-F208        TO  WK-WJYR-F208
                MOVE  HK-WJYR-F208     TO  JYR-F208
****************行番号
                MOVE  WJYR-F302        TO  WK-WJYR-F302
                MOVE  HK-WJYR-F302     TO  JYR-F302
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
     CLOSE     CVCSG001  EDJYURF.
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
         DISPLAY "## NJH9111B RD-CNT = " RD-CNT  UPON CONS
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

```
