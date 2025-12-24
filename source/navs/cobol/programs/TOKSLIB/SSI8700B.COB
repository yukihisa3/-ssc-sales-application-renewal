****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　ＪＥＤＩＣＯＳデータ変換　　　　　*
*    作成日／更新日　　　：　2007/05/28                        *
*    作成者／更新者　　　：　松野　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＪＥＤＩＣＯＳにて受信したデータ　*
*                            をＪＣＡフォーマットに変換する。　*
*    作成日／更新日　　　：　2008/08/28                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
*    作成日／更新日　　　：　2010/02/05                        *
*    作成者／更新者　　　：　大野　　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　受信データの変更に対応　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI8700B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          07/05/28.
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
     SELECT   DCMSIHDT   ASSIGN    TO        DA-01-S-DCMSIHDT
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    EDI-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*支払ヘッダレコード
     SELECT   SITHD87   ASSIGN    TO        DA-01-VI-SITHD871
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SIH-F01   SIH-F02
                        FILE  STATUS   IS   SIH-STATUS.
*支払ヘッダ明細
     SELECT   SITME87   ASSIGN    TO        DA-01-VI-SITME871
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SIM-F01   SIM-F02
                                            SIM-F09   SIM-F03
                                            SIM-F04   SIM-F05
                        FILE  STATUS   IS   SIM-STATUS.
*支払ヘッダテイル
     SELECT   SITTL87   ASSIGN    TO        DA-01-VI-SITTL871
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
 FD  DCMSIHDT
                        BLOCK CONTAINS      3    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  EDI-REC.
     03  EDI-01                  PIC  X(02).
     03  EDI-02                  PIC  X(1280).
******************************************************************
*    支払ヘッダレコード
******************************************************************
 FD  SITHD87            LABEL RECORD   IS   STANDARD.
     COPY     SITHD87   OF        XFDLIB
              JOINING   SIH       PREFIX.
******************************************************************
*    支払明細レコード
******************************************************************
 FD  SITME87            LABEL RECORD   IS   STANDARD.
     COPY     SITME87   OF        XFDLIB
              JOINING   SIM       PREFIX.
******************************************************************
*    支払テイルレコード
******************************************************************
 FD  SITTL87            LABEL RECORD   IS   STANDARD.
     COPY     SITTL87   OF        XFDLIB
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
     COPY   DCMSITHD OF XFDLIB  JOINING   HED  AS   PREFIX.
*明細情報格納領域
     COPY   DCMSITME OF XFDLIB  JOINING   MEI  AS   PREFIX.
*テイル情報格納領域
     COPY   DCMSITTL OF XFDLIB  JOINING   TAL  AS   PREFIX.
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
 01  SITHD87-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SITME87-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SITTL87-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-TOK-F81              PIC  X(02)     VALUE  SPACE.
 01  WK-JOH-F17              PIC  9(02)     VALUE  ZERO.
 01  WK-DENNO.
   03  WK-DENNO1             PIC  X(03)     VALUE  SPACE.
   03  WK-DENNO2             PIC  X(06)     VALUE  SPACE.
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
         05  ST-PG          PIC   X(08)  VALUE "SSI8700B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI8700B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSI8700B".
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
 01  PARA-OUT-CNT1          PIC   9(07).
 01  PARA-OUT-CNT2          PIC   9(07).
 01  PARA-OUT-CNT3          PIC   9(07).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-OUT-CNT1
                                       PARA-OUT-CNT2
                                       PARA-OUT-CNT3.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DCMSIHDT.
     MOVE      "DCMSIHDT "   TO   AB-FILE.
     MOVE      EDI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITHD87.
     MOVE      "SITHD871"   TO   AB-FILE.
     MOVE      SIH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITME87.
     MOVE      "SITME871"    TO   AB-FILE.
     MOVE      SIM-STATUS    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITTL87.
     MOVE      "SITTL871"    TO   AB-FILE.
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
     OPEN     INPUT     DCMSIHDT.
     OPEN     I-O       SITHD87  SITME87  SITTL87.
     OPEN     OUTPUT    SIHARASF.
     DISPLAY  MSG-START UPON CONS.
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
     PERFORM  DCMSIHDT-READ-SEC.
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
     PERFORM DCMSIHDT-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 DCMSIHDT-READ-SEC      SECTION.
*
     MOVE "DCMSIHDT-READ-SEC" TO       S-NAME.
*
     READ     DCMSIHDT
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
 DCMSIHDT-READ-EXIT.
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
*2010/02/05 変更開始
*    取引先ＣＤ
     DISPLAY "HED-F071 = " HED-F071  UPON CONS.
     EVALUATE  HED-F071
         WHEN  000056   MOVE    56  TO  SIH-F02
*        EVALUATE  HED-F06
*            WHEN  02   MOVE   883  TO  SIH-F02
*            WHEN  03   MOVE   882  TO  SIH-F02
*            WHEN  04   MOVE   880  TO  SIH-F02
*        END-EVALUATE
*
         WHEN  000250   MOVE   250  TO  SIH-F02
*        EVALUATE  HED-F06
*            WHEN  02   MOVE   14273  TO  SIH-F02
*            WHEN  03   MOVE   14272  TO  SIH-F02
*            WHEN  04   MOVE   1427   TO  SIH-F02
*        END-EVALUATE
*
*        WHEN  013938   MOVE   13938  TO  SIH-F02
*        WHEN  017137   MOVE   17137  TO  SIH-F02
*        WHEN  100403   MOVE   100403 TO  SIH-F02
*        WHEN  100441   MOVE   100441 TO  SIH-F02
*        WHEN  100427   MOVE   100427 TO  SIH-F02
     END-EVALUATE.
*ファイル存在チェック
     READ  SITHD87
           INVALID      MOVE "INV"    TO  SITHD87-INV-FLG
           NOT  INVALID MOVE SPACE    TO  SITHD87-INV-FLG
     END-READ.
*
     IF    SITHD87-INV-FLG  =  SPACE
           GO              TO   MEISAI-HEAD-EXIT
     END-IF.
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
     EVALUATE  MEI-F022
         WHEN  000880
         EVALUATE  MEI-F091
             WHEN  02   MOVE   883  TO  SIM-F02  SSF-F01
             WHEN  03   MOVE   882  TO  SIM-F02  SSF-F01
             WHEN  04   MOVE   880  TO  SIM-F02  SSF-F01
         END-EVALUATE

         WHEN  001427
         EVALUATE  MEI-F091
             WHEN  02   MOVE   14273  TO  SIM-F02  SSF-F01
             WHEN  03   MOVE   14272  TO  SIM-F02  SSF-F01
             WHEN  04   MOVE   1427   TO  SIM-F02  SSF-F01
         END-EVALUATE

         WHEN  013938   MOVE   13938  TO  SIM-F02  SSF-F01
         WHEN  017137   MOVE   17137  TO  SIM-F02  SSF-F01
         WHEN  100403   MOVE   100403 TO  SIM-F02  SSF-F01
         WHEN  100441   MOVE   100441 TO  SIM-F02  SSF-F01
         WHEN  100427   MOVE   100427 TO  SIM-F02  SSF-F01
     END-EVALUATE.
*    店舗ＣＤ
     MOVE      MEI-F14     TO   SIM-F03  SSF-F02.
*    納品日
     MOVE      MEI-F08     TO   SIM-F04  SSF-F03.
*    伝票番号
     EVALUATE  MEI-F022
*****EVALUATE  HED-F07
         WHEN  013938
         WHEN  017137
         WHEN  100403
         WHEN  100441
         WHEN  100427
               MOVE      MEI-F12(4:6) TO  WK-DENNO2
               MOVE      MEI-F14(2:3) TO  WK-DENNO1
               MOVE      WK-DENNO    TO   SIM-F05 SSF-F04
         WHEN  OTHER
***************MOVE      MEI-F12(1:7) TO   SIM-F05
               MOVE      MEI-F12     TO   SIM-F05
               MOVE      SIM-F05     TO   SSF-F04
     END-EVALUATE.
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
*****DISPLAY "SIM-F05 = " SIM-F05 UPON CONS.
*ファイル存在チェック
     READ  SITME87
           INVALID      MOVE "INV"    TO  SITME87-INV-FLG
           NOT  INVALID MOVE SPACE    TO  SITME87-INV-FLG
     END-READ.
*
     IF     SIM-F05  NOT = ZERO
     AND    SIM-F05  NUMERIC
            WRITE  SSF-REC
            IF  SITME87-INV-FLG = "INV"
                WRITE  SIM-REC
            END-IF
     END-IF.
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
     EVALUATE  MEI-F022
         WHEN  000880
               EVALUATE  MEI-F091
                   WHEN  02   MOVE   883  TO  SIM-F02
                   WHEN  03   MOVE   882  TO  SIM-F02
                   WHEN  04   MOVE   880  TO  SIM-F02
               END-EVALUATE
               MOVE      MEI-F13(1:7) TO   SIM-F05
         WHEN  001427
               EVALUATE  MEI-F091
                   WHEN  02   MOVE   14273  TO  SIM-F02
                   WHEN  03   MOVE   14272  TO  SIM-F02
                   WHEN  04   MOVE   1427   TO  SIM-F02
               END-EVALUATE
               MOVE      MEI-F13(1:7) TO   SIM-F05
         WHEN  013938   MOVE   13938  TO  SIM-F02
               MOVE      MEI-F13      TO   SIM-F05
         WHEN  017137   MOVE   17137  TO  SIM-F02
               MOVE      MEI-F13      TO   SIM-F05
         WHEN  100403   MOVE   100403 TO  SIM-F02
               MOVE      MEI-F13      TO   SIM-F05
         WHEN  100441   MOVE   100441 TO  SIM-F02
               MOVE      MEI-F13      TO   SIM-F05
         WHEN  100427   MOVE   100427 TO  SIM-F02
               MOVE      MEI-F13      TO   SIM-F05
     END-EVALUATE.
*    店舗ＣＤ
     MOVE      MEI-F11(1:4) TO  SIM-F03.
*    納品日
     MOVE      MEI-F09     TO   SIM-F04.
*    伝票番号
*****MOVE      MEI-F13(1:7) TO   SIM-F05.
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
*ファイル存在チェック
     READ  SITME87
           INVALID      MOVE "INV"    TO  SITME87-INV-FLG
           NOT  INVALID MOVE SPACE    TO  SITME87-INV-FLG
     END-READ.
     IF     SIM-F05        NUMERIC
     AND    SITME87-INV-FLG = "INV"
            WRITE  SIM-REC
     END-IF.
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
     EVALUATE  HED-F071
         WHEN  000056   MOVE    56  TO  SIT-F02
*        EVALUATE  HED-F06
*            WHEN  02   MOVE   883  TO  SIT-F02
*            WHEN  03   MOVE   882  TO  SIT-F02
*            WHEN  04   MOVE   880  TO  SIT-F02
*        END-EVALUATE
*
         WHEN  000250   MOVE   250  TO  SIT-F02
*        EVALUATE  HED-F06
*            WHEN  02   MOVE   14273  TO  SIT-F02
*            WHEN  03   MOVE   14272  TO  SIT-F02
*            WHEN  04   MOVE   1427   TO  SIT-F02
*        END-EVALUATE
*
*        WHEN  013938   MOVE   13938  TO  SIT-F02
*        WHEN  017137   MOVE   17137  TO  SIT-F02
*        WHEN  100403   MOVE   100403 TO  SIT-F02
*        WHEN  100441   MOVE   100441 TO  SIT-F02
*        WHEN  100427   MOVE   100427 TO  SIT-F02
     END-EVALUATE.
*ファイル存在チェック
     READ  SITTL87
           INVALID      MOVE "INV"    TO  SITTL87-INV-FLG
           NOT  INVALID MOVE SPACE    TO  SITTL87-INV-FLG
     END-READ.
*
     IF    SITTL87-INV-FLG  =  SPACE
           GO              TO   MEISAI-TAIL-EXIT
     END-IF.
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
     CLOSE     DCMSIHDT  SITHD87  SITME87  SITTL87  SIHARASF.
*
     DISPLAY NC"＃全件　ＣＮＴ＝"  RD-CNT    UPON CONS.
     DISPLAY NC"＃ヘッダＣＮＴ＝"  SIH-CNT   UPON CONS.
     DISPLAY NC"＃明細　ＣＮＴ＝"  SIM-CNT   UPON CONS.
     DISPLAY NC"＃テイルＣＮＴ＝"  SIT-CNT   UPON CONS.
*## 2008/08/28 内部統制対応
     MOVE    SIH-CNT    TO         PARA-OUT-CNT1.
     MOVE    SIM-CNT    TO         PARA-OUT-CNT2.
     MOVE    SIT-CNT    TO         PARA-OUT-CNT3.
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
