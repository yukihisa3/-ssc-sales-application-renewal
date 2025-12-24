****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト物品受領書　　　　　*
*    業務名　　　　　　　：　受領処理                         *
*    モジュール名　　　　：　受領アンマッチリスト発行          *
*    作成日／更新日　　　：　10/10/08                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受領アンマッチリストを発行する。  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3770L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/08.
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
                        YA-21 IS PITCH-20-YKBAI  *> 2.0ピッチ、
                        YB    IS PITCH-15        *> 1.5ピッチ
                        YB-21 IS PITCH-15-YKBAI. *> 1.5ピッチ、
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<受領アンマッチデータ　 >>*********************************
     SELECT   NFUNMAF            ASSIGN    TO   DA-01-VI-NFUNMAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F01  JYU-F02
                                                JYU-F03
                                 STATUS         JYU-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
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
*    FILE = ナフコ　　受領アンマッチデータ                     *
*--------------------------------------------------------------*
 FD  NFUNMAF            LABEL RECORD   IS   STANDARD.
     COPY     NFUNMAF   OF        XFDLIB
              JOINING   JYU       PREFIX.
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
 01  CHK-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JYU-STATUS                   PIC       X(02).
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
*
 01  WK-AREA.
     03  IX1                      PIC       9(02)  VALUE  ZERO.
     03  SET-FLG                  PIC       X(01)  VALUE  SPACE.
     03  PRT-FLG                  PIC       X(01)  VALUE  SPACE.
*
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(03)  VALUE  ZERO.
     03  BRK-DENNO                PIC       9(08)  VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-SURYO                 PIC       9(09)V9(01).
     03  WK-GENKA                 PIC       9(09)V9(02).
     03  WK-KINGAKU               PIC       9(09).
*
 01  WK-DATE-HENKAN.
     03  WK-HEN-YYYY              PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-HEN-MM                PIC       9(02)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-HEN-DD                PIC       9(02)  VALUE  ZERO.
*
 01  WK-KEY.
     03  WK-HATYU-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-NOUHN-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-TENPO-CD              PIC       9(04)  VALUE  ZERO.
     03  WK-DENPYO-NO             PIC       9(07)  VALUE  ZERO.
     03  WK-DENPYO-NO1            PIC       9(07)  VALUE  ZERO.
*
 01  WK-CHG.
     05  FILLER         PIC   X(01)  VALUE "0".
     05  FILLER         PIC   N(01)  VALUE NC"０".
     05  FILLER         PIC   X(01)  VALUE "1".
     05  FILLER         PIC   N(01)  VALUE NC"１".
     05  FILLER         PIC   X(01)  VALUE "2".
     05  FILLER         PIC   N(01)  VALUE NC"２".
     05  FILLER         PIC   X(01)  VALUE "3".
     05  FILLER         PIC   N(01)  VALUE NC"３".
     05  FILLER         PIC   X(01)  VALUE "4".
     05  FILLER         PIC   N(01)  VALUE NC"４".
     05  FILLER         PIC   X(01)  VALUE "5".
     05  FILLER         PIC   N(01)  VALUE NC"５".
     05  FILLER         PIC   X(01)  VALUE "6".
     05  FILLER         PIC   N(01)  VALUE NC"６".
     05  FILLER         PIC   X(01)  VALUE "7".
     05  FILLER         PIC   N(01)  VALUE NC"７".
     05  FILLER         PIC   X(01)  VALUE "8".
     05  FILLER         PIC   N(01)  VALUE NC"８".
     05  FILLER         PIC   X(01)  VALUE "9".
     05  FILLER         PIC   N(01)  VALUE NC"９".
     05  FILLER         PIC   X(01)  VALUE "A".
     05  FILLER         PIC   N(01)  VALUE NC"Ａ".
     05  FILLER         PIC   X(01)  VALUE "B".
     05  FILLER         PIC   N(01)  VALUE NC"Ｂ".
     05  FILLER         PIC   X(01)  VALUE "C".
     05  FILLER         PIC   N(01)  VALUE NC"Ｃ".
     05  FILLER         PIC   X(01)  VALUE "D".
     05  FILLER         PIC   N(01)  VALUE NC"Ｄ".
     05  FILLER         PIC   X(01)  VALUE "E".
     05  FILLER         PIC   N(01)  VALUE NC"Ｅ".
     05  FILLER         PIC   X(01)  VALUE "F".
     05  FILLER         PIC   N(01)  VALUE NC"Ｆ".
     05  FILLER         PIC   X(01)  VALUE "G".
     05  FILLER         PIC   N(01)  VALUE NC"Ｇ".
     05  FILLER         PIC   X(01)  VALUE "H".
     05  FILLER         PIC   N(01)  VALUE NC"Ｈ".
     05  FILLER         PIC   X(01)  VALUE "I".
     05  FILLER         PIC   N(01)  VALUE NC"Ｉ".
     05  FILLER         PIC   X(01)  VALUE "J".
     05  FILLER         PIC   N(01)  VALUE NC"Ｊ".
     05  FILLER         PIC   X(01)  VALUE "K".
     05  FILLER         PIC   N(01)  VALUE NC"Ｋ".
     05  FILLER         PIC   X(01)  VALUE "L".
     05  FILLER         PIC   N(01)  VALUE NC"Ｌ".
     05  FILLER         PIC   X(01)  VALUE "M".
     05  FILLER         PIC   N(01)  VALUE NC"Ｍ".
     05  FILLER         PIC   X(01)  VALUE "N".
     05  FILLER         PIC   N(01)  VALUE NC"Ｎ".
     05  FILLER         PIC   X(01)  VALUE "O".
     05  FILLER         PIC   N(01)  VALUE NC"Ｏ".
     05  FILLER         PIC   X(01)  VALUE "P".
     05  FILLER         PIC   N(01)  VALUE NC"Ｐ".
     05  FILLER         PIC   X(01)  VALUE "Q".
     05  FILLER         PIC   N(01)  VALUE NC"Ｑ".
     05  FILLER         PIC   X(01)  VALUE "R".
     05  FILLER         PIC   N(01)  VALUE NC"Ｒ".
     05  FILLER         PIC   X(01)  VALUE "S".
     05  FILLER         PIC   N(01)  VALUE NC"Ｓ".
     05  FILLER         PIC   X(01)  VALUE "T".
     05  FILLER         PIC   N(01)  VALUE NC"Ｔ".
     05  FILLER         PIC   X(01)  VALUE "U".
     05  FILLER         PIC   N(01)  VALUE NC"Ｕ".
     05  FILLER         PIC   X(01)  VALUE "V".
     05  FILLER         PIC   N(01)  VALUE NC"Ｖ".
     05  FILLER         PIC   X(01)  VALUE "W".
     05  FILLER         PIC   N(01)  VALUE NC"Ｗ".
     05  FILLER         PIC   X(01)  VALUE "X".
     05  FILLER         PIC   N(01)  VALUE NC"Ｘ".
     05  FILLER         PIC   X(01)  VALUE "Y".
     05  FILLER         PIC   N(01)  VALUE NC"Ｙ".
     05  FILLER         PIC   X(01)  VALUE "Z".
     05  FILLER         PIC   N(01)  VALUE NC"Ｚ".
*
 01  TBL-NAT                REDEFINES  WK-CHG.
     03  TBL-NATR           OCCURS  36.
         05  TBL-NAT1       PIC   X(01).
         05  TBL-NAT2       PIC   N(01).
*
 01  WK-GOKEI.
     03  WK-DENPYO-KEI            PIC       9(08)  VALUE  ZERO.
     03  WK-TENPO-KEI             PIC       9(08)  VALUE  ZERO.
     03  WK-SOGOKEI               PIC       9(08)  VALUE  ZERO.
     03  WK-HATYU-SU              PIC      S9(06)V9 VALUE ZERO.
     03  WK-KENSYU-SU             PIC      S9(06)V9 VALUE ZERO.
     03  WK-GENKA-KIN             PIC      S9(08)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3770L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3770L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3770L".
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
 01  HD01          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(08)  VALUE  "SSY3770L".
     03  FILLER                PIC  X(39)  VALUE  SPACE.
     03  FILLER                PIC  N(14)  VALUE
                               NC"＜　受領アンマッチリスト　＞".
     03  FILLER                PIC  X(38)  VALUE  SPACE.
     03  HD01-YY               PIC  9999.
     03  FILLER                PIC  N(01)  VALUE  NC"年".
     03  HD01-MM               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"月".
     03  HD01-DD               PIC  Z9.
     03  FILLER                PIC  N(01)  VALUE  NC"日".
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  HD01-PCNT             PIC  ZZ9.
     03  FILLER                PIC  N(02)  VALUE  NC"頁".
***** 見出し行２
 01  HD02          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE
                               NC"出荷日：".
     03  HD02-STYMD            PIC  9(08).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "-".
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD02-EDYMD            PIC  9(08).
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行３
 01  HD03          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報".
     03  FILLER               PIC  X(05)  VALUE  SPACE.
*
     03  FILLER               PIC  N(05)  VALUE  NC"＜－－－－".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"出".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"荷".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"情".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"報".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"－－－－＞".
     03  FILLER               PIC  X(03)  VALUE  SPACE.
*
     03  FILLER               PIC  N(05)  VALUE  NC"＜－－－－".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"受".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"領".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"情".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"報".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"－－－－＞".
*
 01  HD04          CHARACTER      TYPE IS     PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"伝票番号".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行".
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"出荷日".
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"数　量".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"仕入計上".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"数　量".
     03  FILLER               PIC  X(04)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価単価".
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額".
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"伝票".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"原価".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"売価".
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
*
***** 明細行１
 01  MD01            CHARACTER      TYPE IS     PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENCD               PIC       XXX.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(10).
     03  FILLER                   PIC       X(185) VALUE  SPACE.
*
***** 明細行２
 01  MD02             CHARACTER      TYPE IS     PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-DENNO               PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-GYONO               PIC       99.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-SYU-YMD             PIC       99999999.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-SYU-SURYO           PIC       -,---,--9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SYU-GENKA           PIC       -,---,--9.99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SYU-GENKIN          PIC       ---,---,--9.
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  MD02-JYU-KEIYMD          PIC       99999999.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD02-JYU-SURYO           PIC       -,---,--9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-JYU-GENKA           PIC       -,---,--9.99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-JYU-GENKIN          PIC       ---,---,--9.
     03  FILLER                   PIC       X(04)  VALUE  SPACE.
     03  MD02-JYU-DEN             PIC       N(01).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-JYU-SU              PIC       N(01).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-JYU-GEN             PIC       N(01).
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD02-JYU-BAI             PIC       N(01).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-STYMD             PIC   9(08).
 01  PARA-EDYMD             PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-TORICD
                                           PARA-STYMD
                                           PARA-EDYMD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFUNMAF.
     MOVE     "NFUNMAF "          TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
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
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3770L-START         SECTION.
*
     MOVE   "SSY3770L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3770L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFUNMAF.
     OPEN     INPUT     TENMS1.
     OPEN     OUTPUT    PRINTF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99                  TO   L-CNT.
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
*
     PERFORM  NFUNMAF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　受領アンマッチデータ読み込み
****************************************************************
 NFUNMAF-RD-SEC             SECTION.
*
     MOVE    "NFUNMAF-RD-SEC"    TO   S-NAME.
*
     READ     NFUNMAF
          AT END
              MOVE     "END"      TO        END-FLG
     END-READ.
*
 NFUNMAF-RD-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       L-CNT  >=  58
              MOVE   SPACE   TO   P-REC
              WRITE  P-REC   AFTER PAGE
     END-IF.
*システム日付セット
     MOVE     SYS-YY         TO   HD01-YY.
     MOVE     SYS-MM         TO   HD01-MM.
     MOVE     SYS-DD         TO   HD01-DD.
     MOVE     PARA-STYMD     TO   HD02-STYMD.
     MOVE     PARA-EDYMD     TO   HD02-EDYMD.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     2.
     WRITE    P-REC     FROM      SEN1      AFTER     2.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     9         TO        L-CNT.
     MOVE     "CHK"     TO        CHK-FLG.
*
 MIDASI-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*  改頁チェック
     IF       L-CNT     >    58
              PERFORM  MIDASI-SEC
              MOVE    "1"         TO    PRT-FLG
              MOVE    ZERO        TO    BRK-DENNO
     END-IF.
*
*  店舗ＣＤブレイク時
     IF       JYU-F01   NOT =   BRK-TENCD   OR
              PRT-FLG       =   "1"
*      線出力
              IF    L-CNT    >  9      AND
                    PRT-FLG  =  SPACE
                    WRITE    P-REC      FROM  SEN2  AFTER 1
                    ADD      1          TO    L-CNT
              END-IF
*
              MOVE     JYU-F01    TO    MD01-TENCD
*
              MOVE     PARA-TORICD        TO  TEN-F52
              MOVE     JYU-F01            TO  TEN-F011
              READ    TENMS1
                 INVALID  KEY
                     MOVE   ALL  NC"＊"   TO  MD01-TENMEI
                 NOT INVALID  KEY
                     MOVE    TEN-F02      TO  MD01-TENMEI
              END-READ
*
              WRITE    P-REC      FROM  MD01
              ADD      1          TO    L-CNT
*
              MOVE     JYU-F01    TO    BRK-TENCD
              MOVE     SPACE      TO    PRT-FLG
              MOVE     ZERO       TO    BRK-DENNO
     END-IF.
*
*  伝票番号ブレイク時
     IF       JYU-F02   NOT =  BRK-DENNO
              MOVE      JYU-F02    TO        MD02-DENNO
              MOVE      JYU-F02    TO        BRK-DENNO
     ELSE
              MOVE      SPACE      TO        MD02-DENNO
     END-IF.
*  行番号
     MOVE     JYU-F03              TO        MD02-GYONO.
*--------------
*  出荷情報　
*--------------
*   出荷日　　
     MOVE     JYU-F11              TO        MD02-SYU-YMD.
*   数量　　
     COMPUTE  WK-SURYO    =    JYU-F05  /  10.
     MOVE     WK-SURYO             TO        MD02-SYU-SURYO.
*   原価単価
     COMPUTE  WK-GENKA    =    JYU-F06  /  100.
     MOVE     WK-GENKA             TO        MD02-SYU-GENKA.
*   原価金額
     COMPUTE  WK-KINGAKU  =    WK-GENKA    *    WK-SURYO.
     MOVE     WK-KINGAKU           TO        MD02-SYU-GENKIN.
*--------------
*  受領情報　
*--------------
*   仕入計上日
     MOVE     JYU-F12              TO        MD02-JYU-KEIYMD.
*   数量　　
     COMPUTE  WK-SURYO    =    JYU-F08  /  10.
     MOVE     WK-SURYO             TO        MD02-JYU-SURYO.
*   原価単価
     COMPUTE  WK-GENKA    =    JYU-F09  /  100.
     MOVE     WK-GENKA             TO        MD02-JYU-GENKA.
*   原価金額
     COMPUTE  WK-KINGAKU  =    WK-GENKA    *    WK-SURYO.
     MOVE     WK-KINGAKU           TO        MD02-JYU-GENKIN.
*
* 伝票エラー区分変換
     MOVE   SPACE                  TO        SET-FLG.
     PERFORM   VARYING  IX1  FROM  1  BY  1
               UNTIL   IX1  >  36  OR  SET-FLG = "1"
        IF    JYU-F13   =   TBL-NAT1 (IX1)
              MOVE    TBL-NAT2 (IX1)  TO   MD02-JYU-DEN
              MOVE    "1"             TO   SET-FLG
        END-IF
     END-PERFORM.
*
     IF   SET-FLG    =   SPACE
          MOVE   SPACE                TO   MD02-JYU-DEN
     END-IF.
*
* 数量エラー区分変換
     MOVE   SPACE                  TO        SET-FLG.
     PERFORM   VARYING  IX1  FROM  1  BY  1
               UNTIL   IX1  >  36  OR  SET-FLG = "1"
        IF    JYU-F14   =   TBL-NAT1 (IX1)
              MOVE    TBL-NAT2 (IX1)  TO   MD02-JYU-SU
              MOVE    "1"             TO   SET-FLG
        END-IF
     END-PERFORM.
*
     IF   SET-FLG    =   SPACE
          MOVE   SPACE                TO   MD02-JYU-SU
     END-IF.
*
* 原価エラー区分変換
     MOVE   SPACE                  TO        SET-FLG.
     PERFORM   VARYING  IX1  FROM  1  BY  1
               UNTIL   IX1  >  36  OR  SET-FLG = "1"
        IF    JYU-F15   =   TBL-NAT1 (IX1)
              MOVE    TBL-NAT2 (IX1)  TO   MD02-JYU-GEN
              MOVE    "1"             TO   SET-FLG
        END-IF
     END-PERFORM.
*
     IF   SET-FLG    =   SPACE
          MOVE   SPACE                TO   MD02-JYU-GEN
     END-IF.
*
* 売価エラー区分変換
     MOVE   SPACE                  TO        SET-FLG.
     PERFORM   VARYING  IX1  FROM  1  BY  1
               UNTIL   IX1  >  36  OR  SET-FLG = "1"
        IF    JYU-F16   =   TBL-NAT1 (IX1)
              MOVE    TBL-NAT2 (IX1)  TO   MD02-JYU-BAI
              MOVE    "1"             TO   SET-FLG
        END-IF
     END-PERFORM.
*
     IF   SET-FLG    =   SPACE
          MOVE   SPACE                TO   MD02-JYU-BAI
     END-IF.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD02  AFTER 1.
     ADD      1              TO   L-CNT.
*
*    次レコード読込み
     PERFORM  NFUNMAF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    NFUNMAF   TENMS1    PRINTF.
*
 END-EXIT.
     EXIT.
*
