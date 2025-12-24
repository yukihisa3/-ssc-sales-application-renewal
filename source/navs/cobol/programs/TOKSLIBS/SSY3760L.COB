****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷情報作成エラーリスト　　*
*    業務名　　　　　　　：　出荷処理                         *
*    モジュール名　　　　：　出荷情報作成エラーリスト          *
*    作成日／更新日　　　：　10/10/12                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　出荷情報作成エラーリストを発行する*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3760L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/12.
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
****<<出荷情報作成エラーＦ　 >>*********************************
     SELECT   NFSERRL1            ASSIGN    TO   DA-01-VI-NFSERRL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    SYU-F01
                                                SYU-F02
                                                SYU-F03
                                                SYU-F04
                                                SYU-F05
                                                SYU-F06
                                                SYU-F07
                                                SYU-F08
                                                SYU-F09
                                 STATUS         SYU-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<商品変換テーブル　　　 >>*********************************
     SELECT   HSHOTBL             ASSIGN    TO   DA-01-VI-SHOTBL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SHT-F01  SHT-F02
                                 STATUS         SHT-STATUS.
*
****<<ナフコ用商品名称マスタ >>*********************************
     SELECT   NFMEIMS             ASSIGN    TO   DA-01-VI-NFMEIMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SHO-F01
                                 STATUS         SHO-STATUS.
*
****<<条件マスタ　　　　　　 >>*********************************
     SELECT   JYOKEN             ASSIGN    TO   DA-01-VI-JYOKEN1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    JYO-F01  JYO-F02
                                 STATUS         JYO-STATUS.
*
****<<作場マスタ　　　　　　 >>*********************************
     SELECT   SAKUBAF            ASSIGN    TO   DA-01-VI-SAKUBAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    SAK-F01
                                 STATUS         SAK-STATUS.
*
*****<<  プリント　Ｆ   >>**************************************
     SELECT   PRINTF    ASSIGN    TO        LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　出荷情報エラーファイル                   *
*--------------------------------------------------------------*
 FD  NFSERRL1            LABEL RECORD   IS   STANDARD.
     COPY     NFSERRL1   OF        XFDLIB
              JOINING   SYU       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = 店舗マスタ　　　　　　　　　                       *
*--------------------------------------------------------------*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　商品変換ＴＢＬ　　　                     *
*--------------------------------------------------------------*
 FD  HSHOTBL             LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL    OF        XFDLIB
              JOINING   SHT       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　商品名称マスタ　　　                     *
*--------------------------------------------------------------*
 FD  NFMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     NFMEIMS    OF        XFDLIB
              JOINING   SHO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　条件マスタ　　　　　                     *
*--------------------------------------------------------------*
 FD  JYOKEN              LABEL RECORD   IS   STANDARD.
     COPY     JYOKEN1    OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　作場マスタ　　　　　                     *
*--------------------------------------------------------------*
 FD  SAKUBAF             LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAF    OF        XFDLIB
              JOINING   SAK       PREFIX.
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
*
**** ステイタス　エリア
 01  SYU-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  SHT-STATUS                   PIC       X(02).
 01  SHO-STATUS                   PIC       X(02).
 01  JYO-STATUS                   PIC       X(02).
 01  SAK-STATUS                   PIC       X(02).
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
 01  WK-BACHINO.
     03  WK-BACHI-YMD             PIC       9(08).
     03  WK-BACHI-TIME            PIC       9(04).
     03  WK-BACHI-TORICD          PIC       9(08).
*
 01  WK-AREA.
     03  WK-SURYO                 PIC       9(09)V9(01).
     03  WK-GENKA                 PIC       9(09)V9(02).
     03  WK-KINGAKU               PIC       9(09).
*
 01  WK-ERR.
     03  WK-ERR1                  PIC       N(10).
     03  WK-ERR2                  PIC       N(10).
*
 01  WK-YMD.
     03  WK-YY                    PIC       9(04)  VALUE  ZERO.
     03  WK-MM                    PIC       9(02)  VALUE  ZERO.
     03  WK-DD                    PIC       9(02)  VALUE  ZERO.
*
 01  WK-YMD-R.
     03  WK-YY-R                  PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-MM-R                  PIC       9(02)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "/".
     03  WK-DD-R                  PIC       9(02)  VALUE  ZERO.
*
 01  WK-KEY.
     03  WK-HATYU-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-NOUHN-DATE            PIC       9(08)  VALUE  ZERO.
     03  WK-TENPO-CD              PIC       9(04)  VALUE  ZERO.
     03  WK-DENPYO-NO             PIC       9(07)  VALUE  ZERO.
     03  WK-DENPYO-NO1            PIC       9(07)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3760L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3760L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3760L".
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
     03  FILLER                PIC  X(08)  VALUE  "SSY3760L".
     03  FILLER                PIC  X(30)  VALUE  SPACE.
     03  FILLER                PIC  N(14)  VALUE
                               NC"＜　出荷情報エラーリスト　＞".
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
     03  FILLER                PIC  X(34)  VALUE  SPACE.
     03  FILLER                PIC  N(06)  VALUE
                               NC"（管理番号：".
     03  HD02-KANRINO          PIC  X(08).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD02-BACH-YMD         PIC  X(08).
     03  FILLER                PIC  X(01)  VALUE  "-".
     03  HD02-BACH-TIME        PIC  X(04).
     03  FILLER                PIC  X(01)  VALUE  "-".
     03  HD02-BACH-TORICD      PIC  X(08).
     03  FILLER                PIC  N(01)  VALUE  NC"）".
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行３
 01  HD03          CHARACTER      TYPE IS     PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"作場情報".
     03  FILLER               PIC  X(25)  VALUE  SPACE.
*
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報".
     03  FILLER               PIC  X(13)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"出荷日　".
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"商品情報".
     03  FILLER               PIC  X(28)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"入数".
     03  FILLER               PIC  X(11)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"箱数".
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(05)  VALUE  NC"エラー理由".
*
 01  HD04          CHARACTER      TYPE IS     PITCH-15.
     03  FILLER               PIC  X(51)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店着日　".
     03  FILLER               PIC  X(39)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"数量".
     03  FILLER               PIC  X(13)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"金額".
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
 01  MD01            CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SAKUBA              PIC       XX.
     03  FILLER                   PIC       X(29)  VALUE  SPACE.
     03  MD01-TENCD               PIC       X(05).
     03  FILLER                   PIC       X(14)  VALUE  SPACE.
     03  MD01-SKYMD               PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SHOCD               PIC       X(13).
     03  FILLER                   PIC       X(18)  VALUE  SPACE.
     03  MD01-IRISU               PIC       ZZ,ZZ9.9.
     03  FILLER                   PIC       X(05)  VALUE  SPACE.
     03  MD01-HAKOSU              PIC       ZZZ,ZZ9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-ERR1                PIC       N(12).
     03  FILLER                   PIC       X(185) VALUE  SPACE.
*
***** 明細行２
 01  MD02             CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SKBMEI              PIC       N(20).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-TENMEI              PIC       N(12).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-TNYMD               PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SHOMEI              PIC       N(20).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SURYO               PIC       ZZ,ZZ9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-KINGAKU             PIC       Z,ZZZ,ZZZ,ZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-ERR2                PIC       N(12).
     03  FILLER                   PIC       X(185) VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-KBN               PIC   X(01).
 01  PARA-KANRINO           PIC   X(08).
 01  PARA-BACHINO-YMD       PIC   X(08).
 01  PARA-BACHINO-TIME      PIC   X(04).
 01  PARA-BACHINO-TORI      PIC   X(08).
 01  PARA-SAKBCD            PIC   X(02).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-KBN
                                           PARA-KANRINO
                                           PARA-BACHINO-YMD
                                           PARA-BACHINO-TIME
                                           PARA-BACHINO-TORI
                                           PARA-SAKBCD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFSERRL1.
     MOVE     "NFSERRL1 "          TO        ERR-FL-ID.
     MOVE     SYU-STATUS          TO        ERR-STCD.
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
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HSHOTBL.
     MOVE     "HSHOTBL "          TO        ERR-FL-ID.
     MOVE     SHT-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFMEIMS.
     MOVE     "NFMEIMS "          TO        ERR-FL-ID.
     MOVE     SHO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC5         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE JYOKEN.
     MOVE     "JYOKEN  "          TO        ERR-FL-ID.
     MOVE     JYO-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC6         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SAKUBAF.
     MOVE     "SAKUBAF "          TO        ERR-FL-ID.
     MOVE     SAK-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3760L-START         SECTION.
*
     MOVE   "SSY3760L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3760L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     NFSERRL1.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     HSHOTBL.
     OPEN     INPUT     NFMEIMS.
     OPEN     INPUT     JYOKEN.
     OPEN     INPUT     SAKUBAF.
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
     PERFORM  NFSERRL1-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　出荷情報作成エラーＦ読込
****************************************************************
 NFSERRL1-RD-SEC             SECTION.
*
     MOVE    "NFSERRL1-RD-SEC"    TO   S-NAME.
*
     READ     NFSERRL1
          AT END
              MOVE     "END"      TO        END-FLG
     END-READ.
*
 NFSERRL1-RD-EXIT.
     EXIT.
*
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
*システム日付セット
     MOVE     SYS-YY           TO   HD01-YY.
     MOVE     SYS-MM           TO   HD01-MM.
     MOVE     SYS-DD           TO   HD01-DD.
     MOVE     PARA-KANRINO     TO   HD02-KANRINO.
     MOVE     PARA-BACHINO-YMD  TO  HD02-BACH-YMD.
     MOVE     PARA-BACHINO-TIME TO  HD02-BACH-TIME.
     MOVE     PARA-BACHINO-TORI TO  HD02-BACH-TORICD.
     MOVE     PARA-BACHINO-TORI TO  WK-BACHI-TORICD.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      HD03      AFTER     2.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     7         TO        L-CNT.
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
     IF       L-CNT     >=   58
              PERFORM  MIDASI-SEC
              MOVE    "1"         TO    PRT-FLG
              MOVE    ZERO        TO    BRK-DENNO
     END-IF.
*
*  明細１行目転送
     MOVE     SYU-F05              TO        MD01-SAKUBA.
     MOVE     SYU-F06              TO        MD01-TENCD.
     MOVE     SYU-F07              TO        WK-YMD.
     MOVE     WK-YY                TO        WK-YY-R.
     MOVE     WK-MM                TO        WK-MM-R.
     MOVE     WK-DD                TO        WK-DD-R.
     MOVE     WK-YMD-R             TO        MD01-SKYMD.
     MOVE     SYU-F09              TO        MD01-SHOCD.
     MOVE     SYU-F11              TO        MD01-IRISU.
     MOVE     SYU-F13              TO        MD01-HAKOSU.
*
 MAIN-001.
*明細２行目転送
*  作場名称取得
     MOVE    SYU-F05              TO   SAK-F01.
*
     READ     SAKUBAF
          INVALID  KEY
******        MOVE  ALL NC"＊"    TO   MD02-SKBMEI
              MOVE  SPACE         TO   MD02-SKBMEI
          NOT INVALID  KEY
              MOVE  SAK-F02       TO   MD02-SKBMEI
     END-READ.
*
*　店舗名称取得
     MOVE    WK-BACHI-TORICD      TO   TEN-F52.
     MOVE    SYU-F06              TO   TEN-F011.
*
     READ     TENMS1
          INVALID  KEY
              MOVE  ALL NC"＊"    TO   MD02-TENMEI
          NOT INVALID  KEY
              MOVE  TEN-F03       TO   MD02-TENMEI
     END-READ.
*
     MOVE     SYU-F08              TO        WK-YMD.
     MOVE     WK-YY                TO        WK-YY-R.
     MOVE     WK-MM                TO        WK-MM-R.
     MOVE     WK-DD                TO        WK-DD-R.
     MOVE     WK-YMD-R             TO        MD02-TNYMD.
*
 MAIN-002.
*　商品名称取得
*    MOVE    WK-BACHI-TORICD      TO   SHT-F01.
*    MOVE    SYU-F09              TO   SHT-F02.
*
*    READ     HSHOTBL
*         INVALID  KEY
*             MOVE  ALL NC"＊"    TO   MD02-SHOMEI
*         NOT INVALID  KEY
*             MOVE    SHT-F031    TO   SHO-F01
*             READ     NFMEIMS
*                  INVALID  KEY
*                     MOVE  ALL NC"＊"    TO   MD02-SHOMEI
*                  NOT INVALID  KEY
*                     MOVE  SHO-F05       TO   MD02-SHOMEI
*             END-READ
*    END-READ.
*
     MOVE    SYU-F09              TO   SHO-F01.
*
     READ    NFMEIMS
             INVALID  KEY
                     MOVE  ALL NC"＊"    TO   MD02-SHOMEI
             NOT INVALID  KEY
                     MOVE  SHO-F05       TO   MD02-SHOMEI
     END-READ.
*
 MAIN-003.
*    COMPUTE  WK-SURYO     =  SYU-F12   / 10.
*    MOVE     WK-SURYO             TO        MD02-SURYO.
     MOVE     SYU-F12              TO        MD02-SURYO.
*    COMPUTE  WK-KINGAKU   =  SHO-F11   * WK-SURYO.
     COMPUTE  WK-KINGAKU   =  SHO-F11   * SYU-F12.
     MOVE     WK-KINGAKU           TO        MD02-KINGAKU.
*
*　エラー理由取得
     MOVE     "88"                 TO        JYO-F01.
     MOVE     SYU-F14              TO        JYO-F02(1:1)
*
     READ     JYOKEN
          INVALID  KEY
              MOVE  ALL NC"＊"    TO   MD01-ERR1
              MOVE  ALL NC"＊"    TO   MD02-ERR2
          NOT INVALID  KEY
              MOVE  JYO-F03       TO   WK-ERR
              MOVE  WK-ERR1       TO   MD01-ERR1
              MOVE  WK-ERR2       TO   MD02-ERR2
     END-READ.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD01  AFTER 1.
     WRITE  P-REC  FROM  MD02  AFTER 1.
     WRITE  P-REC  FROM  SEN2  AFTER 2.
     ADD      4              TO   L-CNT.
*
*    次レコード読込み
     PERFORM  NFSERRL1-RD-SEC.
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
     CLOSE    NFSERRL1   TENMS1    NFMEIMS  HSHOTBL
              JYOKEN     SAKUBAF   PRINTF.
*
 END-EXIT.
     EXIT.
*
