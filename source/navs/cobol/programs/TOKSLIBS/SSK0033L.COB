****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ケーヨー伝票レス　　　　　　　　　*
*    業務名　　　　　　　：　ケーヨー伝票レス                  *
*    モジュール名　　　　：　受領書アンマッチリスト発行　      *
*    作成日／更新日　　　：　14/03/17                          *
*    作成者／更新者　　　：　NAV三浦
*    処理概要　　　　　　：　受領アンマッチリストを出力する。  *
*    作成日／更新日　　　：　14/04/10                          *
*    作成者／更新者　　　：　NAV高橋
*    処理概要　　　　　　：　アンマッチデータのキー変更　　　  *
*    　　　　　　　　　　　　出力レイアウト変更　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSK0033L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          14/03/10.
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
                        YA-21 IS PITCH-20-YKBAI  *> 2.0ピッチ、
                        YB    IS PITCH-15        *> 1.5ピッチ
                        YB-21 IS PITCH-15-YKBAI  *> 1.5ピッチ、
                        YB-21 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<受領アンマッチデータ >>*********************************
     SELECT   KEIUMCL1           ASSIGN    TO   DA-01-VI-KEIUMCL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
*********************************2014/04/10 NAV ST
*********************************RECORD  KEY    UMC-F01
*********************************               UMC-F02
*********************************               UMC-F03
                                 RECORD  KEY    UMC-F01
                                                UMC-F06
                                                UMC-F05
                                                UMC-F02
                                                UMC-F03
*********************************2014/04/10 NAV ED
                                 STATUS         UMC-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
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
*    FILE = ケーヨー　受領アンマッチデータ　　　　　　
*--------------------------------------------------------------*
 FD  KEIUMCL1           LABEL RECORD   IS   STANDARD.
     COPY     KEIUMCF   OF        XFDLIB
              JOINING   UMC       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
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
 01  TENMS1-INV-FLG               PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  UMC-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
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
 01  WK-BACHINO.
     03  WK-BACHI-YMD             PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TIME            PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TORICD          PIC       9(08)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC      S9(05)  VALUE  ZERO.
*****2014/04/10 NAV ST
     03  BRK-NOUDT                PIC      S9(08)  VALUE  ZERO.
     03  BRK-DENKU                PIC       X(02)  VALUE  SPACE.
*****2014/04/10 NAV ED
     03  BRK-DENNO                PIC      S9(09)  VALUE  ZERO.
 01  WRT-KEY.
     03  WRT-TENCD                PIC      S9(05)  VALUE  ZERO.
*****2014/04/10 NAV ST
     03  WRT-NOUDT                PIC      S9(08)  VALUE  ZERO.
     03  WRT-DENKU                PIC       X(02)  VALUE  SPACE.
*****2014/04/10 NAV ED
     03  WRT-DENNO                PIC      S9(09)  VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-SHUKASU               PIC      S9(06).
     03  WK-KENSHUSU              PIC      S9(06).
     03  WK-GENKA                 PIC      S9(09).
     03  WK-SASU                  PIC      S9(07).
     03  WK-SHUKASUR              PIC      S9(05)V9(01).
     03  WK-KENSHUSUR             PIC      S9(05)V9(01).
     03  WK-GENKAR                PIC      S9(07)V9(02).
     03  WK-KINGAKU               PIC      S9(10).
     03  WK-SASUR                 PIC      S9(05)V9(02).
*
*--------------
 01  WK-DENKEI.
     03  WK-SHUKASU-DEN           PIC      S9(06)V9.
     03  WK-KENSHUSU-DEN          PIC      S9(06)V9.
     03  WK-KINGAKU-DEN           PIC      S9(09).
*
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSK0033L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSK0033L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSK0033L".
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
     03  FILLER                PIC  X(08)  VALUE  "SSK0033L".
     03  FILLER                PIC  X(19)  VALUE  SPACE.
     03  FILLER                PIC  N(16)
                               VALUE
               NC"＜ケーヨー受領アンマッチリスト＞"
                               CHARACTER  TYPE  IS PITCH-22.
     03  FILLER                PIC  X(22)  VALUE  SPACE.
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
     03  FILLER                PIC  X(120)  VALUE  SPACE.
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
     03  FILLER                PIC  N(04)  VALUE  NC"納品日："
                               CHARACTER   TYPE IS   PITCH-20.
     03  HD03-NFYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NFMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NFDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  N(01)  VALUE  NC"～"
                               CHARACTER  TYPE IS PITCH-20.
     03  HD03-NTYY             PIC  9999   VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NTMM             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(01)  VALUE  "/".
     03  HD03-NTDD             PIC  99     VALUE  ZERO.
     03  FILLER                PIC  X(02)  VALUE  SPACE.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(70)  VALUE  SPACE.
     03  FILLER               PIC  N(11)  VALUE
                                   NC"＜　出　荷　情　報　＞"
                               CHARACTER   TYPE IS   PITCH-15.
     03  FILLER               PIC  N(11)  VALUE
                                   NC"＜　検　収　情　報　＞"
                               CHARACTER   TYPE IS   PITCH-15.
***** 見出し行５
******************************2014/04/10 NAV ST レイアウト変更
 01  HD05.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(09)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"納品日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"伝区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"検収日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"ＪＡＮＣＤ"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"原単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"原単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(06) VALUE  NC"メッセージ１"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(06) VALUE  NC"メッセージ２"
                              CHARACTER  TYPE IS PITCH-20.
******************************2014/04/10 NAV ED レイアウト変更
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
******************************2014/04/10 NAV ST レイアウト変更
 01  MD01                         CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENCD               PIC       9999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(06)
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-MEISAI1.
         05  MD01-NOHINBI     PIC       99999999.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-DENKU-S     PIC       X(02).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-DENKU-K     PIC       X(02).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-KENSHUBI    PIC       99999999.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-DENNO       PIC       999999999.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
     03  MD01-MEISAI2.
         05  MD01-GYO         PIC       Z9.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-JANCD       PIC       X(13).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-SHUKASU     PIC       -,---,--9.
         05  MD01-GENKA-S     PIC       -,---,--9.
         05  MD01-KENSHUSU    PIC       -,---,--9.
         05  MD01-GENKA-K     PIC       -,---,--9.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-MSG1        PIC       N(10)
                              CHARACTER  TYPE IS PITCH-15.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD01-MSG2        PIC       N(10)
                              CHARACTER  TYPE IS PITCH-15.
 01  MD02                     CHARACTER   TYPE IS   PITCH-15.
     03  FILLER               PIC       X(16)  VALUE  SPACE.
     03  MD02-MEISAI1.
         05  MD02-NOHINBI     PIC       99999999.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-DENKU-S     PIC       X(02).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-DENKU-K     PIC       X(02).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-KENSHUBI    PIC       99999999.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-DENNO       PIC       999999999.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
     03  MD02-MEISAI2.
         05  MD02-GYO         PIC       Z9.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-JANCD       PIC       X(13).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-SHUKASU     PIC       -,---,--9.
         05  MD02-GENKA-S     PIC       -,---,--9.
         05  MD02-KENSHUSU    PIC       -,---,--9.
         05  MD02-GENKA-K     PIC       -,---,--9.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-MSG1        PIC       N(10)
                              CHARACTER  TYPE IS PITCH-15.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD02-MSG2        PIC       N(10)
                              CHARACTER  TYPE IS PITCH-15.
 01  MD03                     CHARACTER   TYPE IS   PITCH-15.
     03  FILLER               PIC       X(50)  VALUE  SPACE.
     03  MD03-MEISAI2.
         05  MD03-GYO         PIC       Z9.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD03-JANCD       PIC       X(13).
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD03-SHUKASU     PIC       -,---,--9.
         05  MD03-GENKA-S     PIC       -,---,--9.
         05  MD03-KENSHUSU    PIC       -,---,--9.
         05  MD03-GENKA-K     PIC       -,---,--9.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD03-MSG1        PIC       N(10)
                              CHARACTER  TYPE IS PITCH-15.
         05  FILLER           PIC       X(01)  VALUE  SPACE.
         05  MD03-MSG2        PIC       N(10)
                              CHARACTER  TYPE IS PITCH-15.
******************************2014/04/10 NAV ED レイアウト変更
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-SKBN              PIC   X(01).
 01  PARA-DKBN              PIC   X(01).
 01  PARA-DFROM             PIC   9(08).
 01  PARA-DTO               PIC   9(08).
 01  PARA-KKBN              PIC   X(01).
 01  PARA-TANFROM           PIC   X(02).
 01  PARA-TANTO             PIC   X(02).
 01  PARA-DENK1             PIC   X(02).
 01  PARA-DENK2             PIC   X(02).
 01  PARA-DENK3             PIC   X(02).
 01  PARA-DENK4             PIC   X(02).
 01  PARA-DENK5             PIC   X(02).
 01  PARA-TENFROM           PIC   9(05).
 01  PARA-TENTO             PIC   9(05).
 01  PARA-DENNFROM          PIC   9(09).
 01  PARA-DENNTO            PIC   9(09).
 01  PARA-SKBFROM           PIC   X(02).
 01  PARA-SKBTO             PIC   X(02).
 01  PARA-DENKFROM          PIC   X(02).
 01  PARA-DENKTO            PIC   X(02).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-SKBN
                                           PARA-DKBN
                                           PARA-DFROM
                                           PARA-DTO
                                           PARA-KKBN
                                           PARA-TANFROM
                                           PARA-TANTO
                                           PARA-DENK1
                                           PARA-DENK2
                                           PARA-DENK3
                                           PARA-DENK4
                                           PARA-DENK5
                                           PARA-TENFROM
                                           PARA-TENTO
                                           PARA-DENNFROM
                                           PARA-DENNTO
                                           PARA-SKBFROM
                                           PARA-SKBTO
                                           PARA-DENKFROM
                                           PARA-DENKTO.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           KEIUMCL1.
     MOVE     "KEIUMCL1"          TO        ERR-FL-ID.
     MOVE     UMC-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     MOVE     4000                TO   PROGRAM-STATUS.
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
     MOVE     4000                TO   PROGRAM-STATUS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSK0033L-START         SECTION.
*
     MOVE   "SSK0033L-START"      TO   S-NAME.
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
 SSK0033L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     KEIUMCL1.
     OPEN     INPUT     TENMS1.
     OPEN     OUTPUT    PRINTF.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
*    MOVE     1              TO    PRT-FLG.
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
     MOVE  SPACE                TO   UMC-REC.
     INITIALIZE                      UMC-REC.
*
     START  KEIUMCL1  KEY  >=   UMC-F01  UMC-F06  UMC-F05
                                UMC-F02  UMC-F03
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  KEIUMCL1-RD-SEC.
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
     PERFORM   MIDASISET-SEC.
*　ブレイクキー設定
     MOVE  UMC-F01    TO       BRK-TENCD.
*****2014/04/10 NAV ST
     MOVE  UMC-F06    TO       BRK-NOUDT.
     MOVE  UMC-F05    TO       BRK-DENKU.
*****2014/04/10 NAV ED
     MOVE  UMC-F02    TO       BRK-DENNO.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ケーヨー　受領累積データ読み込み　　　
****************************************************************
 KEIUMCL1-RD-SEC            SECTION.
*
     MOVE    "KEIUMCL1-RD-SEC"    TO   S-NAME.
*
     READ     KEIUMCL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    KEIUMCL1-RD-EXIT
     END-READ.
*
*
 KEIUMCL1-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
*  店舗・納品日・伝区（サカタ）・伝票番号ブレイク時
     IF      ( UMC-F01   NOT =   BRK-TENCD )  OR
*************2014/04/10 NAV ST ↓追加
             ( UMC-F06   NOT =   BRK-NOUDT )  OR
             ( UMC-F05   NOT =   BRK-DENKU )  OR
             ( UMC-F02   NOT =   BRK-DENNO )
*************2014/04/10 NAV ED ↑追加
              IF  ( UMC-F01   NOT =   BRK-TENCD )
                  MOVE  SPACE         TO    RD1-FLG
              END-IF
              MOVE  UMC-F01       TO    BRK-TENCD
*************2014/04/10 NAV ST ↓追加
              MOVE  UMC-F06       TO    BRK-NOUDT
              MOVE  UMC-F05       TO    BRK-DENKU
*************2014/04/10 NAV ED ↑追加
              MOVE  UMC-F02       TO    BRK-DENNO
     END-IF.
*  明細行編集１
     MOVE     UMC-F01           TO    MD01-TENCD
*  店舗正式名
     MOVE     UMC-F01           TO    TEN-F011
     MOVE     173               TO    TEN-F52
     IF  RD1-FLG = SPACE
         PERFORM  TENMS1-READ-SEC
         IF  TENMS1-INV-FLG = "INV"
           MOVE ALL NC"＊"      TO    MD01-TENMEI
         ELSE
           MOVE TEN-F03         TO    MD01-TENMEI
         END-IF
         MOVE  "1"              TO    RD1-FLG
     END-IF.
*  伝票番号
     MOVE     UMC-F02              TO        MD01-DENNO.
*  伝区
     MOVE     UMC-F05              TO        MD01-DENKU-S.
     MOVE     UMC-F04              TO        MD01-DENKU-K.
*  納品日
*****MOVE     UMC-F12              TO        MD01-NOHINBI.
     MOVE     UMC-F06              TO        MD01-NOHINBI.
*  検収日
     MOVE     UMC-F07              TO        MD01-KENSHUBI.
*  行番
     MOVE     UMC-F03              TO        MD01-GYO.
*  JANCD
     MOVE     UMC-F08              TO        MD01-JANCD.
*  出荷数
     MOVE     UMC-F11              TO        MD01-SHUKASU.
*  原価単価
     MOVE     UMC-F13              TO        MD01-GENKA-S.
*  検収数
     MOVE     UMC-F09              TO        MD01-KENSHUSU.
*  原価単価
     MOVE     UMC-F10              TO        MD01-GENKA-K.
*  メッセージ１
     MOVE     UMC-F14              TO        MD01-MSG1.
*  メッセージ２
     MOVE     UMC-F15              TO        MD01-MSG2.
*
     MOVE     MD01-MEISAI1         TO        MD02-MEISAI1.
     MOVE     MD01-MEISAI2         TO        MD02-MEISAI2.
     MOVE     MD01-MEISAI2         TO        MD03-MEISAI2.
*--------------
*  明細行出力
*--------------
*  改頁チェック
      IF  L-CNT     =    99
             PERFORM  MIDASI-SEC
             PERFORM  HED-SEC
             MOVE  UMC-F01       TO    WRT-TENCD
*************2014/04/10 NAV ST ↓追加
             MOVE  UMC-F06       TO    WRT-NOUDT
             MOVE  UMC-F05       TO    WRT-DENKU
*************2014/04/10 NAV ED ↑追加
             MOVE  UMC-F02       TO    WRT-DENNO
     ELSE
         IF   ( UMC-F01   NOT =   WRT-TENCD )
             PERFORM   DENKEI-SEC
             PERFORM   HED-SEC
             MOVE  UMC-F01       TO    WRT-TENCD
*************2014/04/10 NAV ST ↓追加
             MOVE  UMC-F06       TO    WRT-NOUDT
             MOVE  UMC-F05       TO    WRT-DENKU
*************2014/04/10 NAV ED ↑追加
             MOVE  UMC-F02       TO    WRT-DENNO
         ELSE
*************2014/04/11 NAV ST ↓変更
*************IF  ( UMC-F02   NOT =   WRT-DENNO )
*                PERFORM   DENKEI-SEC
**               WRITE  P-REC  FROM  MD02  AFTER 1
*                ADD    1            TO    L-CNT
*                MOVE  UMC-F02       TO    WRT-DENNO
*            ELSE
*                IF  L-CNT     =    10
*                    PERFORM   HED-SEC
*                ELSE
*                    IF   L-CNT     >    54
*                       PERFORM  MIDASI-SEC
*                       PERFORM  HED-SEC
*                    ELSE
*                       WRITE  P-REC  FROM  MD03  AFTER 1
*                       ADD    1            TO    L-CNT
*                    END-IF
*                END-IF
*************END-IF
             IF  ( UMC-F02   =   WRT-DENNO )
             AND ( UMC-F05   =   WRT-DENKU )
                 IF  L-CNT     =    10
                     PERFORM   HED-SEC
                 ELSE
                     IF   L-CNT     >    54
                        PERFORM  MIDASI-SEC
                        PERFORM  HED-SEC
                     ELSE
                        WRITE  P-REC  FROM  MD03  AFTER 1
                        ADD    1            TO    L-CNT
                     END-IF
                 END-IF
             ELSE
                 PERFORM   DENKEI-SEC
                 WRITE  P-REC  FROM  MD02  AFTER 1
                 ADD    1            TO    L-CNT
                 MOVE  UMC-F02       TO    WRT-DENNO
                 MOVE  UMC-F05       TO    WRT-DENKU
             END-IF
*************2014/04/11 NAV ST ↑変更
         END-IF
     END-IF.
 KEIUMCL1-RD.
*  改頁チェック
*    次レコード読込み
     PERFORM  KEIUMCL1-RD-SEC.
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
*日付区分・日付
     MOVE     PARA-DFROM(1:4)   TO   HD03-NFYY.
     MOVE     PARA-DFROM(5:2)   TO   HD03-NFMM.
     MOVE     PARA-DFROM(7:2)   TO   HD03-NFDD.
     MOVE     PARA-DTO(1:4)     TO   HD03-NTYY.
     MOVE     PARA-DTO(5:2)     TO   HD03-NTMM.
     MOVE     PARA-DTO(7:2)     TO   HD03-NTDD.
*
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
     MOVE     10         TO        L-CNT.
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
*
 HED-EXIT.
     EXIT.
****************************************************************
*             改行出力                                         *
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*
*  線出力
     IF  L-CNT     >    54
         PERFORM  MIDASI-SEC
     END-IF.
     IF  ( UMC-F01   NOT =   WRT-TENCD )
             WRITE    P-REC     FROM  SEN2  AFTER 1
     ELSE
             WRITE    P-REC     FROM  SEN3  AFTER 1
     END-IF.
     ADD      1         TO    L-CNT
     IF   L-CNT     >    54
          PERFORM  MIDASI-SEC
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
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    KEIUMCL1 TENMS1 PRINTF.
*
 END-EXIT.
     EXIT.
*
