# SSI8300B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSI8300B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2006/10/24                        *
*    作成者／更新者　　　：　高橋　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＪＥＤＩＣＯＳにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI8300B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          06/10/24.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   HOMSIHDT   ASSIGN    TO        DA-01-S-HOMSIHDT
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払ヘッダレコード
     SELECT   SITHD83   ASSIGN    TO        DA-01-VI-SITHD831
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SIH-F01   SIH-F02
                        FILE  STATUS   IS   SIH-STATUS.
*支払ヘッダ明細
     SELECT   SITME83   ASSIGN    TO        DA-01-VI-SITME831
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SIM-F01   SIM-F02
                                            SIM-F09   SIM-F03
                                            SIM-F04   SIM-F05
                        FILE  STATUS   IS   SIM-STATUS.
*支払ヘッダテイル
     SELECT   SITTL83   ASSIGN    TO        DA-01-VI-SITTL831
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SIT-F01   SIT-F02
                        FILE  STATUS   IS   SIT-STATUS.
*支払データ
     SELECT   SIHARASF  ASSIGN    TO        DA-01-S-SIHARASF
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    SSF-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　２５６　  ＢＦ＝　１
******************************************************************
 FD  HOMSIHDT
                        BLOCK CONTAINS      3    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(1278).
******************************************************************
*    支払ヘッダレコード
******************************************************************
 FD  SITHD83            LABEL RECORD   IS   STANDARD.
     COPY     SITHD83   OF        XFDLIB
              JOINING   SIH       PREFIX.
******************************************************************
*    支払明細レコード
******************************************************************
 FD  SITME83            LABEL RECORD   IS   STANDARD.
     COPY     SITME83   OF        XFDLIB
              JOINING   SIM       PREFIX.
******************************************************************
*    支払テイルレコード
******************************************************************
 FD  SITTL83            LABEL RECORD   IS   STANDARD.
     COPY     SITTL83   OF        XFDLIB
              JOINING   SIT       PREFIX.
******************************************************************
*    支払データ
******************************************************************
 FD  SIHARASF
                        BLOCK CONTAINS      40   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     SIHARASF  OF        XFDLIB
              JOINING   SSF       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ヘッダ情報格納領域
     COPY   HOMSITHD OF XFDLIB  JOINING   HED  AS   PREFIX.
*明細情報格納領域
     COPY   HOMSITME OF XFDLIB  JOINING   MEI  AS   PREFIX.
*テイル情報格納領域
     COPY   HOMSITTL OF XFDLIB  JOINING   TAL  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  SIH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SIM-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SIT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  JCA-CNT4                PIC  9(08)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-HOKAIDO              PIC  9(08)     VALUE  ZERO.
 01  WK-TOUHOKU              PIC  9(08)     VALUE  ZERO.
 01  WK-KANTOU               PIC  9(08)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  EDI-STATUS         PIC   X(02).
     03  SIH-STATUS         PIC   X(02).
     03  SIM-STATUS         PIC   X(02).
     03  SIT-STATUS         PIC   X(02).
     03  SSF-STATUS         PIC   X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSI8300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI8300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI8300B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*数量１２桁対応
 01  WK-HENKAN-12           PIC   X(12)  VALUE  SPACE.
 01  WK-HEN-12              PIC   X(12).
 01  WK-HEN-12-R            REDEFINES   WK-HEN-12.
     03  WK-HENSU-12        PIC   9(12).
 01  WK-FUGO12              PIC   X(01)  VALUE  SPACE.
*数量１０桁対応
 01  WK-HENKAN-10           PIC   X(10)  VALUE  SPACE.
 01  WK-HEN-10              PIC   X(10).
 01  WK-HEN-10-R            REDEFINES   WK-HEN-10.
     03  WK-HENSU-10        PIC   9(10).
 01  WK-FUGO10              PIC   X(01)  VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
 LINKAGE                SECTION.
 01  PARA-KBN               PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-KBN.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HOMSIHDT.
     MOVE      "HOMSIHDT "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITHD83.
     MOVE      "SITHD831"   TO   AB-FILE.
     MOVE      SIH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITME83.
     MOVE      "SITME831"    TO   AB-FILE.
     MOVE      SIM-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITTL83.
     MOVE      "SITTL831"    TO   AB-FILE.
     MOVE      SIT-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SIHARASF.
     MOVE      "SIHARASF"    TO   AB-FILE.
     MOVE      SSF-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HOMSIHDT.
     OPEN     I-O       SITHD83  SITME83  SITTL83.
     OPEN     OUTPUT    SIHARASF.
     DISPLAY  MSG-START UPON CONS.
*
     EVALUATE PARA-KBN
         WHEN "1"  MOVE  883      TO   WK-HOKAIDO
                   MOVE  882      TO   WK-TOUHOKU
                   MOVE  880      TO   WK-KANTOU
         WHEN "2"  MOVE  14273    TO   WK-HOKAIDO
                   MOVE  14272    TO   WK-TOUHOKU
                   MOVE  1427     TO   WK-KANTOU
         WHEN OTHER
                   DISPLAY NC"＃取引先判定エラー＃" UPON CONS
                   STOP  RUN
     END-EVALUATE.
*
     MOVE     ZERO      TO        RD-CNT.
     MOVE     ZERO      TO        SIH-CNT SIM-CNT SIT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*
*
     PERFORM  HOMSIHDT-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*    伝票ヘッダレコード
     IF    EDI-01  =   "HD"
           PERFORM  MEISAI-HEAD-SEC
     END-IF.
*
     IF    EDI-01  =   "DT"
           MOVE      SPACE       TO   MEI-REC
           INITIALIZE                 MEI-REC
           MOVE      EDI-REC     TO   MEI-REC
           IF    MEI-F08  NUMERIC
                 MOVE     SPACE  TO   SIM-REC
                 INITIALIZE           SIM-REC
                 PERFORM  MEISAI-SIHARAI-SEC
           END-IF
           IF    MEI-F09  NUMERIC
                 MOVE     SPACE  TO   SIM-REC
                 INITIALIZE           SIM-REC
                 PERFORM  MEISAI-ISAN-SEC
           END-IF
     END-IF.
*
     IF    EDI-01  =   "TR"
           PERFORM  MEISAI-TAIL-SEC
     END-IF.
*
     PERFORM HOMSIHDT-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 HOMSIHDT-READ-SEC      SECTION.
*
     MOVE "HOMSIHDT-READ-SEC" TO       S-NAME.
*
     READ     HOMSIHDT
              AT END
              MOVE     9      TO    END-FLG
              NOT AT END
              ADD      1      TO    RD-CNT
     END-READ.
*
     IF   RD-CNT(6:3) = "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 HOMSIHDT-READ-EXIT.
     EXIT.
****************************************************************
*　　支払ヘッダレコード作成
****************************************************************
 MEISAI-HEAD-SEC    SECTION.
*
***********ワークエリア初期化
     MOVE      SPACE       TO   SIH-REC  HED-REC.
     INITIALIZE                 SIH-REC  HED-REC.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   HED-REC.
*****支払ヘッダ作成
*    締日
     MOVE      HED-F03     TO   SIH-F01.
*    取引先ＣＤ
     EVALUATE  HED-F06
         WHEN  02   MOVE   WK-HOKAIDO  TO  SIH-F02
         WHEN  03   MOVE   WK-TOUHOKU  TO  SIH-F02
         WHEN  04   MOVE   WK-KANTOU   TO  SIH-F02
     END-EVALUATE.
*    支払日
     MOVE      HED-F04     TO   SIH-F03.
*    支払総額
     MOVE      HED-F08     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIH-F05 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIH-F05
     END-IF.
*    支払税額
     MOVE      HED-F09     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIH-F07 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIH-F07
     END-IF.
*    消費税等対象額
     MOVE      HED-F10     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIH-F09 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIH-F09
     END-IF.
*    消費税等税額
     MOVE      HED-F11     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIH-F11 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIH-F11
     END-IF.
*    非課税分支払額
     MOVE      HED-F12     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIH-F13 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIH-F13
     END-IF.
     WRITE  SIH-REC.
     ADD    1              TO   SIH-CNT.
*
 MEISAI-HEAD-EXIT.
     EXIT.
****************************************************************
*　　支払明細レコード作成
****************************************************************
 MEISAI-SIHARAI-SEC SECTION.
*    支払データ初期化
     MOVE      SPACE       TO   SSF-REC.
     INITIALIZE                 SSF-REC.
*****支払ヘッダ作成
*    締日
     MOVE      HED-F03     TO   SIM-F01.
*    取引先ＣＤ
     EVALUATE  HED-F06
         WHEN  02   MOVE   WK-HOKAIDO  TO  SIM-F02  SSF-F01
         WHEN  03   MOVE   WK-TOUHOKU  TO  SIM-F02  SSF-F01
         WHEN  04   MOVE   WK-KANTOU   TO  SIM-F02  SSF-F01
     END-EVALUATE.
*    店舗ＣＤ
     MOVE      MEI-F14     TO   SIM-F03  SSF-F02.
*    納品日
     MOVE      MEI-F08     TO   SIM-F04  SSF-F03.
*    伝票番号
     MOVE      MEI-F12     TO   SIM-F05.
     MOVE      SIM-F05     TO   SSF-F04.
*    支払金額
     MOVE      MEI-F03     TO   WK-HENKAN-10.
     MOVE      MEI-F03     TO   SSF-F05.
     PERFORM   HENKAN-10-SEC.
     IF   WK-FUGO10  =  "-"
          COMPUTE SIM-F07 = WK-HENSU-10  *  -1
          COMPUTE SSF-F05 = WK-HENSU-10  *  -1
     ELSE
          MOVE WK-HENSU-10 TO   SIM-F07
     END-IF.
*    摘要
     EVALUATE  MEI-F10
         WHEN  NC"＊"  MOVE "*"  TO  SIM-F08  SSF-F06
         WHEN  NC"＃"  MOVE "#"  TO  SIM-F08  SSF-F06
         WHEN  NC"ヒ"  MOVE "ﾋ"  TO  SIM-F08  SSF-F06
         WHEN  OTHER   MOVE " "  TO  SIM-F08  SSF-F06
     END-EVALUATE.
*    発注／差異区分
     MOVE      "1"         TO   SIM-F09.
     WRITE  SSF-REC.
     WRITE  SIM-REC.
     ADD    1              TO   SIM-CNT.
*
 MEISAI-SIHARAI-EXIT.
     EXIT.
****************************************************************
*　　支払違算レコード作成
****************************************************************
 MEISAI-ISAN-SEC SECTION.
*
*****支払ヘッダ作成
*    締日
     MOVE      HED-F03     TO   SIM-F01.
*    取引先ＣＤ
     EVALUATE  HED-F06
         WHEN  02   MOVE   WK-HOKAIDO  TO  SIM-F02
         WHEN  03   MOVE   WK-TOUHOKU  TO  SIM-F02
         WHEN  04   MOVE   WK-KANTOU   TO  SIM-F02
     END-EVALUATE.
*    店舗ＣＤ
     MOVE      MEI-F11(1:4) TO  SIM-F03.
*    納品日
     MOVE      MEI-F09     TO   SIM-F04.
*    伝票番号
     MOVE      MEI-F13     TO   SIM-F05.
*    伝票明細金額
     MOVE      MEI-F04     TO   WK-HENKAN-10.
     PERFORM   HENKAN-10-SEC.
     IF   WK-FUGO10  =  "-"
          COMPUTE SIM-F11 = WK-HENSU-10  *  -1
     ELSE
          MOVE WK-HENSU-10 TO   SIM-F07
     END-IF.
*    伝票合計金額
     MOVE      MEI-F05     TO   WK-HENKAN-10.
     PERFORM   HENKAN-10-SEC.
     IF   WK-FUGO10  =  "-"
          COMPUTE SIM-F13 = WK-HENSU-10  *  -1
     ELSE
          MOVE WK-HENSU-10 TO   SIM-F13
     END-IF.
*    請求金額
     MOVE      MEI-F06     TO   WK-HENKAN-10.
     PERFORM   HENKAN-10-SEC.
     IF   WK-FUGO10  =  "-"
          COMPUTE SIM-F15 = WK-HENSU-10  *  -1
     ELSE
          MOVE WK-HENSU-10 TO   SIM-F15
     END-IF.
*    請求差額金額
     MOVE      MEI-F07     TO   WK-HENKAN-10.
     PERFORM   HENKAN-10-SEC.
     IF   WK-FUGO10  =  "-"
          COMPUTE SIM-F17 = WK-HENSU-10  *  -1
     ELSE
          MOVE WK-HENSU-10 TO   SIM-F17
     END-IF.
*    差異区分
     MOVE      MEI-F16     TO   SIM-F18.
*    発注／差異区分
     MOVE      "2"         TO   SIM-F09.
     WRITE  SIM-REC.
     ADD    1              TO   SIM-CNT.
*
 MEISAI-ISAN-EXIT.
     EXIT.
****************************************************************
*　　支払テイルレコード作成
****************************************************************
 MEISAI-TAIL-SEC SECTION.
*
***********ワークエリア初期化
     MOVE      SPACE       TO   SIT-REC  TAL-REC.
     INITIALIZE                 SIT-REC  TAL-REC.
*****ヘッダ情報→ワークにセット
     MOVE      EDI-REC     TO   TAL-REC.
*****支払ヘッダ作成
*    締日
     MOVE      HED-F03     TO   SIT-F01.
*    取引先ＣＤ
     EVALUATE  HED-F06
         WHEN  02   MOVE   WK-HOKAIDO  TO  SIT-F02
         WHEN  03   MOVE   WK-TOUHOKU  TO  SIT-F02
         WHEN  04   MOVE   WK-KANTOU   TO  SIT-F02
     END-EVALUATE.
*    請求金額合計
     MOVE      TAL-F03     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIT-F04 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIT-F04
     END-IF.
*    差異金額合計
     MOVE      TAL-F04     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIT-F06 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIT-F06
     END-IF.
*    支払金額合計
     MOVE      TAL-F05     TO   WK-HENKAN-12.
     PERFORM   HENKAN-12-SEC.
     IF   WK-FUGO12  =  "-"
          COMPUTE SIT-F08 = WK-HENSU-12  *  -1
     ELSE
          MOVE WK-HENSU-12 TO   SIT-F08
     END-IF.
     WRITE  SIT-REC.
     ADD    1              TO   SIT-CNT.
*
 MEISAI-TAIL-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     CLOSE     HOMSIHDT  SITHD83  SITME83  SITTL83  SIHARASF.
*
     DISPLAY NC"＃全件　ＣＮＴ＝"  RD-CNT    UPON CONS.
     DISPLAY NC"＃ヘッダＣＮＴ＝"  SIH-CNT   UPON CONS.
     DISPLAY NC"＃明細　ＣＮＴ＝"  SIM-CNT   UPON CONS.
     DISPLAY NC"＃テイルＣＮＴ＝"  SIT-CNT   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　数量１２桁変換
****************************************************************
 HENKAN-12-SEC   SECTION.
*
     MOVE    SPACE                        TO WK-FUGO12.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 12
             IF   WK-HENKAN-12(IX:1) = "-"
                  MOVE "0"                TO WK-HEN-12(IX:1)
                  MOVE "-"                TO WK-FUGO12
             ELSE
                  MOVE WK-HENKAN-12(IX:1) TO WK-HEN-12(IX:1)
             END-IF
     END-PERFORM.
*
 HENKAN-12-EXIT.
     EXIT.
****************************************************************
*　　　　　　　数量１０桁変換
****************************************************************
 HENKAN-10-SEC   SECTION.
*
     MOVE    SPACE                        TO WK-FUGO10.
     PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > 10
             IF   WK-HENKAN-10(IX:1) = "-"
                  MOVE "0"                TO WK-HEN-10(IX:1)
                  MOVE "-"                TO WK-FUGO10
             ELSE
                  MOVE WK-HENKAN-10(IX:1) TO WK-HEN-10(IX:1)
             END-IF
     END-PERFORM.
*
 HENKAN-10-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
