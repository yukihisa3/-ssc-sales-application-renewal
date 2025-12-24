****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　      *
*    業務名　　　　　　　：　伝票纏め返品　　　　　            *
*    モジュール名　　　　：　伝票纏め結果リスト発行　　　　　　*
*    作成日／更新日　　　：　2017/08/21                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　パラメタを受取り、伝票纏め返品累積*
*                          データより範囲内のリストを出力する。*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SJR0390L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          17/08/21.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS
                        YA    IS PITCH-20        *> 2.0ピッチ
                        YA-22 IS PITCH-22        *> 2.0ピッチ
                        YA-21 IS PITCH-20-YKBAI  *> 2.0ピッチ
                        YB    IS PITCH-15        *> 1.5ピッチ
                        YB-21 IS PITCH-15-YKBAI  *> 1.5ピッチ
                        YB-21 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<伝票纏め返品実績データ >>*******************************
     SELECT   COMRMHF           ASSIGN    TO   DA-01-VI-COMRMHL7
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    RMH-F78
                                                RMH-F79
                                                RMH-F01
                                                RMH-F02
                                                RMH-F03
                                                RMH-F04
                                                RMH-F05
                                 STATUS         RMH-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<取引先マスタ　　　　　　 >>*******************************
     SELECT   TOKMS2            ASSIGN    TO   DA-01-VI-TOKMS2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TOK-F01
                                 STATUS         TOK-STATUS.
*
****<<条件ファル　　　　　　 >>*********************************
     SELECT   JYOKEN1            ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*
*                                                                *
*                                                                *
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = 伝票纏め返品実績データ                             *
*--------------------------------------------------------------*
 FD  COMRMHF           LABEL RECORD   IS   STANDARD.
     COPY     COMRMHF   OF        XFDLIB
              JOINING   RMH       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 取引先マスタ　　　　　　　　　
*--------------------------------------------------------------*
 FD  TOKMS2            LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 条件ファイル　　　　　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN1             LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = プリントファイル                                   *
*--------------------------------------------------------------*
 FD  PRINTF.
 01  P-REC                        PIC       X(200).
*
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
 01  TOKMS2-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
 01  JYOKEN1-INV-FLG              PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  RMH-STATUS                   PIC       X(02).
 01  TOK-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
***** システム時刻ワーク
 01  SYSTEM-TIME.
     03  SYS-HH                   PIC  9(02).
     03  SYS-MN                   PIC  9(02).
     03  SYS-SS                   PIC  9(02).
*
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  RD1-FLG                  PIC       X(01)  VALUE  SPACE.
     03  RD2-FLG                  PIC       X(01)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(05)  VALUE  ZERO.
     03  BRK-DENNO                PIC       9(09)  VALUE  ZERO.
     03  BRK-DENK                 PIC       9(02)  VALUE  ZERO.
     03  BRK-NOHINBI              PIC       9(08)  VALUE  ZERO.
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN             PIC      S9(09)V99.
     03  WK-KINGAKU-DEN           PIC      S9(11).
     03  WK-BAIGAKU-DEN           PIC      S9(11).
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0390L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0390L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SJR0390L".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
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
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
***** 見出し行１
 01  HD01.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(08)  VALUE  "SJR0390L".
     03  FILLER                PIC  X(31)  VALUE  SPACE.
     03  FILLER                PIC  N(11)
                       VALUE NC"＜伝票纏め結果リスト＞"
                               CHARACTER  TYPE  IS PITCH-22.
     03  FILLER                PIC  X(30)  VALUE  SPACE.
     03  HD01-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年"
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD01-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD01-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日"
                               CHARACTER  TYPE IS PITCH-20.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  HD01-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁"
                               CHARACTER  TYPE IS PITCH-20.
***** 見出し行２
 01  HD02.
     03  FILLER                PIC  X(116) VALUE  SPACE.
     03  HD02-HH               PIC  99.
     03  FILLER                PIC  N(01)  VALUE  NC"："
                               CHARACTER  TYPE  IS PITCH-20.
     03  HD02-MN               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"："
                               CHARACTER  TYPE IS PITCH-20.
     03  HD02-SS               PIC  Z9.
***** 見出し行３
 01  HD03.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(06)  VALUE
         NC"バッチ番号："      CHARACTER   TYPE IS   PITCH-20.
     03  HD03-BT-DATE          PIC  9(08).
     03  FILLER                PIC  X(03)  VALUE  " - ".
     03  HD03-BT-TIME          PIC  X(05).
     03  FILLER                PIC  X(03)  VALUE  " - ".
     03  HD03-BT-TOKCD         PIC  9(08).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD03-BT-TOKNM         PIC  N(15)
                               CHARACTER  TYPE IS PITCH-15.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"取引先"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(19)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"出場"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"店舗"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(17)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"相伝区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  X(03)  VALUE  "ｻｶﾀ".
     03  FILLER               PIC  N(02)  VALUE  NC"伝区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"検収日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"実検収日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"作成日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"計上日"
                              CHARACTER  TYPE IS PITCH-20.
***** 見出し行５
 01  HD05.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"量販店商品"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(35)  VALUE  SPACE.
     03  FILLER               PIC  X(02)  VALUE  "( ".
     03  FILLER       PIC  N(09)  VALUE  NC"サカタ商品コード　"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  ")".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  X(01)  VALUE  "(".
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  ")".
     03  FILLER               PIC  N(04)  VALUE  NC"明細備考"
                              CHARACTER  TYPE IS PITCH-20.
*
***** 線
 01  SEN1.
     03  FILLER                   PIC       X(40)  VALUE
         "========================================".
     03  FILLER                   PIC       X(40)  VALUE
         "========================================".
     03  FILLER                   PIC       X(40)  VALUE
         "========================================".
     03  FILLER                   PIC       X(16)  VALUE
         "================".
*
***** 線
 01  SEN2.
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(40)  VALUE
         "----------------------------------------".
     03  FILLER                   PIC       X(16)  VALUE
         "----------------".
***** 空白行
 01  SEN3.
     03  FILLER                   PIC       X(136)  VALUE SPACE.
*
***** 明細行
 01  MD01                         CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-TORICD              PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TORINM              PIC       N(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENNO               PIC       999999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SKBCD               PIC       X(02).
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-TENCD               PIC       99999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(10).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-DENKU               PIC       X(02).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-SKTDENKU            PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SKTDENKUNM          PIC       N(04).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KENSHUBI            PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-JKENSHUBI           PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SAKUSEIBI           PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KEIJOBI             PIC       99999999.
 01  MD02.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-GYO                 PIC       Z9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-JANCD               PIC       X(13).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SHONM1              PIC       X(15).
     03  MD02-SHONM2              PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  FILLER                   PIC       X(01)  VALUE  "(".
     03  MD02-SHOCD               PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-HINTAN1             PIC       X(05).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN2             PIC       X(02).
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  MD02-HINTAN3             PIC       X(01).
     03  FILLER                   PIC       X(01)  VALUE  ")".
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SURYO               PIC       -,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-GENKA               PIC       -,---,--9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-KINGAKU             PIC       ---,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-BIKO                PIC       X(10).
***** 合計行
 01  GK01.
     03  FILLER                   PIC     X(03)  VALUE  SPACE.
     03  GK01-BIKO1               PIC  N(04) VALUE  NC"＜備考："
                                  CHARACTER   TYPE IS   PITCH-20.
     03  GK01-MEISAIBIKO1         PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GK01-MEISAIBIKO2         PIC       X(15).
     03  GK01-BIKO2               PIC   N(01) VALUE  NC"＞"
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC     X(01)  VALUE  SPACE.
     03  GK01-GOKEI        PIC  N(06) VALUE  NC"＜伝票合計＞"
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(16)  VALUE  SPACE.
     03  GK01-SURYO               PIC       -,---,--9.
     03  FILLER                   PIC       X(12)  VALUE  SPACE.
     03  GK01-KINGAKU             PIC       ---,---,--9.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-BT-DATE           PIC   9(08).
 01  PARA-BT-TIME           PIC   9(04).
 01  PARA-BT-TOKCD          PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-BUMON
                                           PARA-TANCD
                                           PARA-BT-DATE
                                           PARA-BT-TIME
                                           PARA-BT-TOKCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           COMRMHF.
     MOVE     "COMRMHF"           TO        ERR-FL-ID.
     MOVE     RMH-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TENMS1.
     MOVE     "TENMS1  "          TO        ERR-FL-ID.
     MOVE     TEN-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE TOKMS2.
     MOVE     "TOKMS2 "          TO        ERR-FL-ID.
     MOVE     TOK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN1.
     MOVE     "JYOKEN1 "          TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE      4000               TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SJR0390L-START         SECTION.
*
     MOVE   "SJR0390L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     IF    END-FLG    NOT =  "END"
           PERFORM    MAIN-SEC  UNTIL     END-FLG   =  "END"
     END-IF.
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SJR0390L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     COMRMHF.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     TOKMS2.
     OPEN     INPUT     JYOKEN1.
     OPEN     OUTPUT    PRINTF.
*
     MOVE     ZERO           TO    WK-DENKEI.
     INITIALIZE                    WK-DENKEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
*
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
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
     ACCEPT    SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   RMH-REC.
     INITIALIZE                      RMH-REC.
*
     MOVE  PARA-BT-DATE         TO   RMH-F78.
     MOVE  PARA-BT-TIME         TO   RMH-F79.
     MOVE  PARA-BT-TOKCD        TO   RMH-F01.
     START  COMRMHF  KEY  >= RMH-F78 RMH-F79 RMH-F01
                             RMH-F02 RMH-F03 RMH-F04
                             RMH-F05
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  COMRMHF-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
*ブレイクキー設定
     MOVE  RMH-F02    TO       BRK-TENCD.
     MOVE  RMH-F04    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　売上累積データ読み込み　　　
****************************************************************
 COMRMHF-RD-SEC            SECTION.
*
     MOVE    "COMRMHF-RD-SEC"    TO   S-NAME.
*
     READ     COMRMHF
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    COMRMHF-RD-EXIT
     END-READ.
*
     IF  PARA-BT-DATE   NOT =  RMH-F78
     OR  PARA-BT-TIME   NOT =  RMH-F79
     OR  PARA-BT-TOKCD  NOT =  RMH-F01
         MOVE     "END"           TO   END-FLG
     END-IF.
*
 COMRMHF-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
*  ブレイク時
     IF      ( RMH-F02   NOT =   BRK-TENCD )  OR
             ( RMH-F04   NOT =   BRK-DENNO )
*        合計
              PERFORM   DENKEI-SEC
              MOVE   ZERO         TO    WK-DENKEI
              INITIALIZE                WK-DENKEI
              MOVE  SPACE         TO    RD1-FLG
              MOVE  "1"           TO    SET-FLG
              MOVE  RMH-F02       TO    BRK-TENCD
              MOVE  RMH-F04       TO    BRK-DENNO
     END-IF.
*  明細行編集１
*  取引先
     MOVE     RMH-F01             TO   MD01-TORICD
*  伝票番号
     MOVE     RMH-F04             TO   MD01-DENNO.
*  出荷場所
     MOVE     RMH-F08             TO   MD01-SKBCD.
*  店舗ＣＤ
     MOVE     RMH-F02             TO   MD01-TENCD
*  相手伝票区分
     MOVE     RMH-F06             TO   MD01-DENKU.
*  サカタ伝票区分
     MOVE     RMH-F23             TO   MD01-SKTDENKU.
*  サカタ伝票区分名
     MOVE     1                   TO   JYO-F01.
     MOVE     RMH-F23             TO   JYO-F02.
     PERFORM  JYOKEN1-READ-SEC.
     IF       JYOKEN1-INV-FLG  =  "INV"
              MOVE ALL NC"＊"     TO   MD01-SKTDENKUNM
     ELSE
              MOVE JYO-F03        TO   MD01-SKTDENKUNM
     END-IF.
*  検収日
     MOVE     RMH-F03             TO   MD01-KENSHUBI.
*  実検収日
     MOVE     RMH-F07             TO   MD01-JKENSHUBI.
*  作成日
     MOVE     RMH-F30             TO   MD01-SAKUSEIBI.
*  計上日
     MOVE     RMH-F86             TO   MD01-KEIJOBI.
     IF  RD1-FLG = SPACE
*  取引先正式名
         MOVE     RMH-F01         TO   TOK-F01
         PERFORM  TOKMS2-READ-SEC
         IF  TOKMS2-INV-FLG = "INV"
           MOVE ALL NC"＊"        TO   MD01-TORINM
         ELSE
           MOVE TOK-F03           TO   MD01-TORINM
         END-IF
*  店舗略名
         MOVE     RMH-F02         TO   TEN-F011
         MOVE     RMH-F01         TO   TEN-F52
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"        TO   MD01-TENMEI
         ELSE
           MOVE TEN-F03           TO   MD01-TENMEI
         END-IF
     END-IF.
*
*  明細行編集２
*  行番
     MOVE     RMH-F05             TO   MD02-GYO.
*  指定商品コード
     MOVE     RMH-F09             TO   MD02-JANCD.
*  商品名
     MOVE     RMH-F14             TO   MD02-SHONM1.
     MOVE     RMH-F15             TO   MD02-SHONM2.
*  サカタ商品
     MOVE     RMH-F10             TO   MD02-SHOCD.
     MOVE     RMH-F11             TO   MD02-HINTAN1.
     MOVE     RMH-F12             TO   MD02-HINTAN2.
     MOVE     RMH-F13             TO   MD02-HINTAN3.
*  数量
     MOVE     RMH-F16             TO   MD02-SURYO.
*  原価単価
     MOVE     RMH-F17             TO   MD02-GENKA.
*  原価金額
     MOVE     RMH-F18             TO   MD02-KINGAKU.
*  備考
     MOVE     RMH-F21             TO   MD02-BIKO.
*--------------
*  伝票計加算
*--------------
*  伝票備考
     MOVE     RMH-F24             TO   GK01-MEISAIBIKO1.
     MOVE     RMH-F25             TO   GK01-MEISAIBIKO2.
*  数量　
     COMPUTE  WK-SURYO-DEN     = WK-SURYO-DEN     +  RMH-F16.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN   = WK-KINGAKU-DEN   +  RMH-F18.
*
*--------------
*  明細行出力
*--------------
*  改頁チェック
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
     IF       SET-FLG   = "1"
              PERFORM  HED-SEC
     END-IF.
     IF       L-CNT     >    54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
     WRITE  P-REC  FROM  MD02  AFTER 1.
     ADD    1            TO    L-CNT.
*    次レコード読込み
     PERFORM  COMRMHF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             見出し編集処理                1.2                *
****************************************************************
 MIDASISET-SEC             SECTION.
*
     MOVE    "MIDASISET-SEC"              TO    S-NAME.
*システム日付・時刻セット
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
     MOVE     SYS-HH            TO   HD02-HH.
     MOVE     SYS-MN            TO   HD02-MN.
     MOVE     SYS-SS            TO   HD02-SS.
*バッチ番号
     MOVE     PARA-BT-DATE      TO   HD03-BT-DATE.
     MOVE     PARA-BT-TIME      TO   HD03-BT-TIME.
     MOVE     PARA-BT-TOKCD     TO   HD03-BT-TOKCD.
*取引先名取得
     MOVE     PARA-BT-TOKCD     TO   TOK-F01.
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG = "INV"
              MOVE    ALL NC"＊"           TO   HD03-BT-TOKNM
     ELSE
              MOVE    TOK-F02              TO   HD03-BT-TOKNM
     END-IF.
*
 MIDASISET-EXIT.
     EXIT.
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       P-CNT  >  ZERO
              MOVE   SPACE   TO   P-REC
              WRITE  P-REC   AFTER PAGE
     END-IF.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     3.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      HD05      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     10        TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
****************************************************************
*             ヘッダ出力処理　              1.3                *
****************************************************************
 HED-SEC             SECTION.
*
     MOVE    "HED-SEC"              TO    S-NAME.
     WRITE    P-REC     FROM      MD01      AFTER     1.
     ADD      1          TO        L-CNT.
     MOVE    SPACE                  TO    SET-FLG.
*
 HED-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >  54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  数量　
     MOVE     WK-SURYO-DEN       TO   GK01-SURYO.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF    END-FLG    NOT =  "END"
         IF       L-CNT  >  54
              PERFORM  MIDASI-SEC
         END-IF
         WRITE    P-REC     FROM  SEN2  AFTER 1
         ADD      1         TO    L-CNT
     END-IF.
*
 DENKEI-EXIT.
     EXIT.
*
***************************************************************
*             店舗マスタ読込
***************************************************************
 TENMS1-READ-SEC        SECTION.
*
     MOVE    "TENMS1-READ-SEC"  TO        S-NAME.
*
     READ     TENMS1
              INVALID      MOVE  "INV"    TO   TENMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TENMS1-INV-FLG
     END-READ.
*
 TENMS1-READ-EXIT.
     EXIT.
***************************************************************
*             取引先マスタ読込
***************************************************************
 TOKMS2-READ-SEC       SECTION.
*
     MOVE    "TOKMS2-READ-SEC" TO        S-NAME.
*
     READ     TOKMS2
              INVALID      MOVE  "INV"    TO   TOKMS2-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
***************************************************************
*             条件ファイル読込
***************************************************************
 JYOKEN1-READ-SEC       SECTION.
*
     MOVE    "JYOKEN1-READ-SEC" TO        S-NAME.
*
     READ     JYOKEN1
              INVALID      MOVE  "INV"    TO   JYOKEN1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   JYOKEN1-INV-FLG
     END-READ.
*
 JYOKEN1-READ-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF      L-CNT   NOT =  99
* 伝票計出力
         IF       L-CNT  >  54
              PERFORM  MIDASI-SEC
              PERFORM  HED-SEC
         END-IF
         PERFORM   DENKEI-SEC
     END-IF.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    COMRMHF TENMS1 TOKMS2 JYOKEN1 PRINTF.
*
 END-EXIT.
     EXIT.
*
