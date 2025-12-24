# SSY4906L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSY4906L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ホーマック物品受領書　　　　　　　*
*    業務名　　　　　　　：　物品受領書                        *
*    モジュール名　　　　：　物品受領書                        *
*    作成日／更新日　　　：　03/04/16                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ホーマック物品受領書を発行する。  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY4906L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/04/16.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS
                        YA        IS        YA
                        YB-21     IS        YB-21.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*****<<ｶﾞｰﾃﾞﾅｰ物品受領データ >>*********************************
     SELECT   HOMJYUF            ASSIGN    TO   DA-01-VI-HOMJYUL2
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F01  JYU-F04
                                                JYU-F02  JYU-F03
                                                JYU-F06  JYU-F07
                                 STATUS         JYU-STATUS.
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
*    FILE = ｶﾞｰﾃﾞﾅｰ物品受領書                                  *
*--------------------------------------------------------------*
 FD  HOMJYUF            LABEL RECORD   IS   STANDARD.
     COPY     HOMJYUF   OF        XFDLIB
              JOINING   JYU       PREFIX.
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
***** カウンタ
 01  P-CNT                        PIC       9(04)  VALUE  ZERO.
 01  L-CNT                        PIC       9(03)  VALUE  ZERO.
 01  CNT-READ                     PIC       9(06)  VALUE  ZERO.
 01  MEI-CNT                      PIC       9(05)  VALUE  ZERO.
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
         05  ST-PG          PIC   X(08)  VALUE "SSY4906L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY4906L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY4906L".
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
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(10)  VALUE
         NC"株式会社サカタのタネ" CHARACTER TYPE IS YA.
     03  FILLER                   PIC  X(26)  VALUE  SPACE.
     03  FILLER                   PIC  N(15)  VALUE
       NC"※ホーマック　　　物品受領書※"
                   CHARACTER      TYPE IS     YB-21.
     03  FILLER                   PIC  X(20)  VALUE  SPACE.
     03  FILLER                   PIC  X(05)  VALUE  "DATE:".
     03  HD01-SYSDATE             PIC  X(10)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  X(05)  VALUE  "PAGE:".
     03  HD01-P-CNT               PIC  ZZ9.
***** 見出し行２
 01  HD02                         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"発注日".
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"納品日".
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(02)  VALUE  NC"店舗".
     03  FILLER                   PIC  X(10)  VALUE  SPACE.
     03  FILLER                   PIC  N(02)  VALUE  NC"伝区".
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(02)  VALUE  NC"伝票".
     03  FILLER                   PIC  X(04)  VALUE  "NO  ".
     03  FILLER                   PIC  N(02)  VALUE  NC"商品".
     03  FILLER                   PIC  X(02)  VALUE  "CD".
     03  FILLER                   PIC  X(08)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"商品名".
*****03  FILLER                   PIC  X(08)  VALUE  SPACE.
*****03  FILLER                   PIC  N(03)  VALUE  NC"商品名".
     03  FILLER                   PIC  X(26)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"発注数".
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"検収数".
     03  FILLER                   PIC  X(04)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"原単価".
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(04) VALUE NC"原価金額".
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(04) VALUE NC"欠品数量".
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
***** 線
 01  SEN3.
     03  FILLER                   PIC       X(106) VALUE  SPACE.
     03  FILLER                   PIC       X(30)  VALUE
         "------------------------------".
*
***** 明細行１
 01  MD01.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-HATYUD              PIC       X(06).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-NOUHND              PIC       X(06).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENCD               PIC       ZZZ9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
*****03  MD01-TENNM               PIC       X(15).
     03  MD01-TENNM               PIC       X(10).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENKU               PIC       99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENNO               PIC       9(07).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SYOCD               PIC       9(13).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SYONM1              PIC       X(15).
     03  MD01-SYONM2              PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-HATYUSU             PIC       ---,--9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KENSYSU             PIC       ---,--9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-GENTANKA            PIC       -,---,--9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-GENKAKIN            PIC       --,---,--9.
     03  FILLER                   PIC       X(03)  VALUE  SPACE.
     03  MD01-KU1                 PIC       X(01).
     03  MD01-KEPPIN              PIC       ---,---.
     03  MD01-KU2                 PIC       X(01).
*
***** 合計１
 01  GK01                         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC       X(103) VALUE  SPACE.
     03  FIILER                   PIC       N(05)  VALUE
                                            NC"　伝票計：".
     03  GK01-DEN-KEI             PIC       --,---,--9.
*
***** 合計２
 01  GK02                         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC       X(103) VALUE  SPACE.
     03  FIILER                   PIC       N(05)  VALUE
                                            NC"店舗合計：".
     03  GK02-TEN-KEI             PIC       --,---,--9.
*
***** 合計３
 01  GK03                         CHARACTER  TYPE  IS  YA.
     03  FILLER                   PIC       X(103) VALUE  SPACE.
     03  FIILER                   PIC       N(05)  VALUE
                                            NC"仕入先計：".
     03  GK03-SOGOKEI             PIC       --,---,--9.
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-TKBN              PIC   9(01).
 01  PARA-OUTNO             PIC   9(01).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-TKBN
                                           PARA-OUTNO.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE HOMJYUF.
     MOVE     "HOMJYUF "          TO        ERR-FL-ID.
     MOVE     JYU-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY4906L-START         SECTION.
*
     MOVE   "SSY4906L-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY4906L-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     HOMJYUF.
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
     PERFORM  HOMJYUF-RD-SEC.
     IF       END-FLG   =   "END"
              DISPLAY NC"＃対象データ無し＃" UPON CONS
     ELSE
              MOVE      JYU-F02    TO        WK-HATYU-DATE
              MOVE      JYU-F03    TO        WK-NOUHN-DATE
              MOVE      JYU-F04    TO        WK-TENPO-CD
              MOVE      JYU-F06    TO        WK-DENPYO-NO
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ホーマック物品受領書データ読込み
****************************************************************
 HOMJYUF-RD-SEC             SECTION.
*
     MOVE    "HOMJYUF-RD-SEC"    TO   S-NAME.
*
     READ     HOMJYUF
          AT END
              MOVE     "END"      TO        END-FLG
     END-READ.
*
 HOMJYUF-RD-EXIT.
     EXIT.
*
****************************************************************
*             見出し出力処理                1.2                *
****************************************************************
 MIDASI-SEC             SECTION.
*
     MOVE    "MIDASI-SEC"              TO    S-NAME.
*改頁
     IF       P-CNT  >=  1
              MOVE   SPACE   TO   P-REC
              WRITE  P-REC   AFTER PAGE
     END-IF.
*システム日付セット
     MOVE     SYS-YY         TO   WK-HEN-YYYY.
     MOVE     SYS-MM         TO   WK-HEN-MM.
     MOVE     SYS-DD         TO   WK-HEN-DD.
     MOVE     WK-DATE-HENKAN TO   HD01-SYSDATE.
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-P-CNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     2.
     WRITE    P-REC     FROM      HD02      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     5         TO        L-CNT.
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
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    店舗合計（発注日、納品日のどちらかがブレイクした場合）
     IF       JYU-F02  =  WK-HATYU-DATE
*****AND      JYU-F03  =  WK-NOUHN-DATE
     AND      JYU-F04  =  WK-TENPO-CD
              CONTINUE
     ELSE
              PERFORM   GOUKEI1-SEC
              PERFORM   GOUKEI2-SEC
              MOVE      JYU-F02    TO        WK-HATYU-DATE
              MOVE      JYU-F03    TO        WK-NOUHN-DATE
              MOVE      JYU-F04    TO        WK-TENPO-CD
              MOVE      JYU-F06    TO        WK-DENPYO-NO
     END-IF.
*    伝票合計（伝票番号がブレイクした場合）
     IF       JYU-F06  =  WK-DENPYO-NO
              CONTINUE
     ELSE
              PERFORM   GOUKEI1-SEC
              MOVE      JYU-F06    TO        WK-DENPYO-NO
     END-IF.
*    明細印字
     PERFORM  MEISAI-SEC.
*    次レコード読込み
     PERFORM  HOMJYUF-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*             明細行出力処理                2.1.1              *
****************************************************************
 MEISAI-SEC             SECTION.
*
     MOVE    "MEISAI-SEC"    TO   S-NAME.
*改頁チェック
     IF       L-CNT     >    60
              PERFORM  MIDASI-SEC
     END-IF.
*初期化
     MOVE     SPACE          TO   MD01.
*ヘッダー印字
     IF       JYU-F06  NOT =  WK-DENPYO-NO1
              MOVE JYU-F06   TO        WK-DENPYO-NO1
              MOVE SPACE     TO        CHK-FLG
     ELSE
              IF  CHK-FLG = "CHK"
                  MOVE SPACE TO        CHK-FLG
              ELSE
                  GO         TO        MEISAI-010
              END-IF
     END-IF.
*発注日
     MOVE     JYU-F02(3:6)   TO   MD01-HATYUD.
*納品日
     MOVE     JYU-F03(3:6)   TO   MD01-NOUHND.
*店舗ＣＤ
     MOVE     JYU-F04        TO   MD01-TENCD.
*店舗名
     MOVE     JYU-F05        TO   MD01-TENNM.
*伝票区分
     MOVE     JYU-F08        TO   MD01-DENKU.
*伝票番号
     MOVE     JYU-F06        TO   MD01-DENNO.
 MEISAI-010.
*商品ＣＤ
     MOVE     JYU-F15        TO   MD01-SYOCD.
*商品名
     MOVE     JYU-F101       TO   MD01-SYONM1.
     MOVE     JYU-F102       TO   MD01-SYONM2.
*発注数
     MOVE     "("            TO   MD01-KU1.
     MOVE     ")"            TO   MD01-KU2.
     EVALUATE JYU-F08
         WHEN 01
              MOVE  JYU-F11  TO   MD01-HATYUSU
              MOVE  JYU-F12  TO   MD01-KENSYSU
              COMPUTE MD01-KEPPIN = JYU-F11  -  JYU-F12
              MOVE  JYU-F13  TO   MD01-GENTANKA
              MOVE  JYU-F14  TO   MD01-GENKAKIN
              ADD   JYU-F14  TO   WK-DENPYO-KEI
              ADD   JYU-F14  TO   WK-TENPO-KEI
              ADD   JYU-F14  TO   WK-SOGOKEI
         WHEN 02 WHEN 03
              COMPUTE WK-HATYU-SU  = JYU-F11 * -1
              MOVE  WK-HATYU-SU  TO  MD01-HATYUSU
              COMPUTE WK-KENSYU-SU = JYU-F12 * -1
              MOVE  WK-KENSYU-SU TO  MD01-KENSYSU
**************MOVE  WK-KENSYU-SU TO  MD01-GENTANKA
              MOVE  JYU-F13      TO  MD01-GENTANKA
              COMPUTE WK-GENKA-KIN = JYU-F14 * -1
              COMPUTE MD01-KEPPIN = WK-HATYU-SU - WK-KENSYU-SU
              MOVE  WK-GENKA-KIN TO  MD01-GENKAKIN
              ADD   WK-GENKA-KIN TO  WK-DENPYO-KEI
              ADD   WK-GENKA-KIN TO  WK-TENPO-KEI
              ADD   WK-GENKA-KIN TO  WK-SOGOKEI
     END-EVALUATE.
*明細行印字
     WRITE  P-REC  FROM  MD01  AFTER 1.
     ADD      1              TO   L-CNT  MEI-CNT.
*
 MEISAI-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF       MEI-CNT  >  ZERO
              PERFORM   GOUKEI1-SEC
              PERFORM   GOUKEI2-SEC
              PERFORM   GOUKEI3-SEC
     END-IF.
*
     CLOSE    HOMJYUF   PRINTF.
*
 END-EXIT.
     EXIT.
*
****************************************************************
*    伝票合計
****************************************************************
 GOUKEI1-SEC            SECTION.
*
     MOVE    "GOUKEI1-SEC"   TO        S-NAME.
*改頁チェック
     IF       L-CNT     >    60
              PERFORM  MIDASI-SEC
     END-IF.
*    伝票合計セット
     MOVE     WK-DENPYO-KEI  TO        GK01-DEN-KEI.
*    伝票合計行印字
     WRITE    P-REC   FROM   GK01  AFTER  1.
*
     ADD      1              TO        L-CNT.
*    伝票合計初期化
     MOVE     ZERO           TO        WK-DENPYO-KEI.
*
 GOUKEI1-END.
     EXIT.
*
****************************************************************
*    店舗合計
****************************************************************
 GOUKEI2-SEC            SECTION.
*
     MOVE    "GOUKEI2-SEC"   TO        S-NAME.
*改頁チェック
     IF       L-CNT     >    60
              PERFORM  MIDASI-SEC
     END-IF.
*    店舗合計セット
     MOVE     WK-TENPO-KEI   TO        GK02-TEN-KEI.
*    線
     WRITE    P-REC   FROM   SEN3  AFTER  1.
*    店舗合計行印字
     WRITE    P-REC   FROM   GK02  AFTER  1.
*    線
     WRITE    P-REC   FROM   SEN2  AFTER  1.
*
     ADD      3              TO        L-CNT.
*    店舗合計初期化
     MOVE     ZERO           TO        WK-TENPO-KEI.
*
 GOUKEI2-END.
     EXIT.
*
****************************************************************
*    仕入先合計
****************************************************************
 GOUKEI3-SEC            SECTION.
*
     MOVE    "GOUKEI3-SEC"   TO        S-NAME.
*改頁チェック
     IF       L-CNT     >    60
              PERFORM  MIDASI-SEC
     END-IF.
*    総合計セット
     MOVE     WK-SOGOKEI     TO        GK03-SOGOKEI.
*    総合計行印字
     WRITE    P-REC   FROM   GK03  AFTER  1.
*
     ADD      1              TO        L-CNT.
*
 GOUKEI3-END.
     EXIT.
********************<<  PUROGRAM  END  >>*************************

```
