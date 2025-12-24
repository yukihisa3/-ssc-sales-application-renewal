# SSY3768N

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3768N.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩシステム　　　　　　*
*    業務名　　　　　　　：　受領処理                          *
*    モジュール名　　　　：　受領書出力　　　　　　　　　      *
*    作成日／更新日　　　：　10/10/15                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受領書を出力する。　　　　　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY3768N.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/15.
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
                        YB-21 IS PITCH-15-YKBAI  *> 1.5ピッチ、
                        YB-21 IS PITCH-30.       *> 3.0ピッチ
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<受領累積マスタ >>*********************************
     SELECT   NFJYURF            ASSIGN    TO   DA-01-VI-NFJYURL3
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JYU-F01  JYU-F05
                                                JYU-F07  JYU-F08
                                 STATUS         JYU-STATUS.
*
****<<店舗マスタ　　　　　　 >>*********************************
     SELECT   TENMS1             ASSIGN    TO   DA-01-VI-TENMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    TEN-F52  TEN-F011
                                 STATUS         TEN-STATUS.
*
****<<受領アンマッチデータ　 >>*********************************
     SELECT   NFUNMAF            ASSIGN    TO   DA-01-VI-NFUNMAL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    UNM-F01  UNM-F02
                                                UNM-F03
                                 STATUS         UNM-STATUS.
*
****<<ナフコ用商品名称マスタ >>*********************************
     SELECT   NFMEIMS             ASSIGN    TO   DA-01-VI-NFMEIMS1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   RANDOM
                                 RECORD  KEY    MEI-F01
                                 STATUS         MEI-STATUS.
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
*    FILE = ナフコ　　受領累積ファイル　　　　　               *
*--------------------------------------------------------------*
 FD  NFJYURF            LABEL RECORD   IS   STANDARD.
     COPY     NFJYURF   OF        XFDLIB
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
*    FILE = ナフコ　　受領アンマッチデータ                     *
*--------------------------------------------------------------*
 FD  NFUNMAF            LABEL RECORD   IS   STANDARD.
     COPY     NFUNMAF   OF        XFDLIB
              JOINING   UNM       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　商品名称マスタ　　　                     *
*--------------------------------------------------------------*
 FD  NFMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     NFMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
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
 01  JYU-STATUS                   PIC       X(02).
 01  UNM-STATUS                   PIC       X(02).
 01  TEN-STATUS                   PIC       X(02).
 01  MEI-STATUS                   PIC       X(02).
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
 01  WK-BACHINO.
     03  WK-BACHI-YMD             PIC       9(08)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TIME            PIC       9(04)  VALUE  ZERO.
     03  FILLER                   PIC       X(01)  VALUE  "-".
     03  WK-BACHI-TORICD          PIC       9(08)  VALUE  ZERO.
*
 01  BRK-KEY.
     03  BRK-TENCD                PIC       9(03)  VALUE  ZERO.
     03  BRK-DENCD                PIC       9(09)  VALUE  ZERO.
*
 01  WK-AREA.
     03  WK-SURYO                 PIC       9(06).
     03  WK-GENKA                 PIC       9(09).
     03  WK-SURYOR                PIC       9(05)V9(01).
     03  WK-GENKAR                PIC       9(07)V9(02).
     03  WK-KINGAKU               PIC       9(10).
*
*--------------
 01  WK-DENKEI.
     03  WK-SURYO-DEN             PIC       9(07)V9.
     03  WK-GENKA-DEN             PIC       9(07)V99.
     03  WK-KINGAKU-DEN           PIC       9(10).
*
 01  WK-TENKEI.
     03  WK-SURYO-TEN             PIC       9(07)V9.
     03  WK-GENKA-TEN             PIC       9(07)V99.
     03  WK-KINGAKU-TEN           PIC       9(10).
*
 01  WK-SOKEI.
     03  WK-SURYO-KEI             PIC       9(07)V9.
     03  WK-GENKA-KEI             PIC       9(07)V99.
     03  WK-KINGAKU-KEI           PIC       9(10).
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3768N".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3768N".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY3768N".
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
     03  FILLER                PIC  X(08)  VALUE  "SSY3768N".
     03  FILLER                PIC  X(44)  VALUE  SPACE.
     03  FILLER                PIC  N(10)
                               VALUE NC"＜　ナフコ受領書　＞"
                               CHARACTER  TYPE  IS PITCH-30.
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
     03  FILLER                PIC  X(02)  VALUE  SPACE.
     03  FILLER                PIC  N(04)  VALUE  NC"計上日："
                               CHARACTER  TYPE IS PITCH-15.
     03  HD02-STYMD            PIC  9(08).
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  FILLER                PIC  X(01)  VALUE  "-"  .
     03  FILLER                PIC  X(01)  VALUE  SPACE.
     03  HD02-EDYMD            PIC  9(08).
     03  FILLER                PIC  X(180) VALUE  SPACE.
***** 見出し行４
 01  HD03.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"店舗情報"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(08)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"計上日"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"伝区"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(02)  VALUE  NC"赤黒"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"伝票_"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(03)  VALUE  SPACE.
     03  FILLER               PIC  N(01)  VALUE  NC"行"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(10)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"商品名"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(12)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"受領数量"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(06)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"原単価"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(07)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"原価金額"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(02)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"受領区分"
                              CHARACTER  TYPE IS PITCH-15.
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"理由区分"
                              CHARACTER  TYPE IS PITCH-15.
***** 見出し行４
 01  HD04.
     03  FILLER               PIC  X(35)  VALUE  SPACE.
     03  FILLER               PIC  X(01)  VALUE  "(".
     03  FILLER               PIC  N(08)  VALUE  SPACE.
     03  FILLER               PIC  N(07)  VALUE
         NC"サカタ商品ＣＤ"   CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(05)  VALUE  SPACE.
     03  FILLER               PIC  X(02)  VALUE  ")".
     03  FILLER               PIC  X(01)  VALUE  SPACE.
     03  FILLER               PIC  N(03)  VALUE  NC"規格名"
                              CHARACTER  TYPE IS PITCH-20.
     03  FILLER               PIC  X(56)  VALUE  SPACE.
     03  FILLER               PIC  N(04)  VALUE  NC"理由ＣＤ"
                              CHARACTER  TYPE IS PITCH-15.
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
***** 明細行
 01  MD01.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENCD               PIC       XXX.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-TENMEI              PIC       N(05)
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KEIJYOBI            PIC       999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENKU               PIC       99.
     03  MD01-DENKU1              PIC       X(01).
     03  MD01-DNKNM               PIC       N(02)
                                  CHARACTER   TYPE IS   PITCH-15.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-AKAKURO             PIC       9.
     03  MDO1-AKAKURO1            PIC       X(01).
     03  MD01-AKAKURONM           PIC       N(01)
                                  CHARACTER   TYPE IS   PITCH-20.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-DENNO               PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-GYO                 PIC       9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SHOCD               PIC       99999999.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-SHONM               PIC       X(15).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-JYURYOSU            PIC       -,---,--9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-GENKA               PIC       -,---,--9.99.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD01-KINGAKU             PIC       ---,---,--9.
     03  FILLER                   PIC       X(02)  VALUE  SPACE.
     03  MD01-JYU                 PIC       99.
     03  MD01-JYU1                PIC       X(01).
     03  MD01-JYUNM               PIC       N(06).
***** 明細行
 01  MD02.
     03  FILLER                   PIC       X(35)  VALUE  SPACE.
     03  MD02-SKT-SYOCDX          PIC       X(01).
     03  MD02-SKT-SYOCD           PIC       X(08).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-SKT-HINTN1          PIC       X(05).
     03  MD02-SKT-HINTNX          PIC       X(01).
     03  MD02-SKT-HINTN2          PIC       X(02).
     03  MD02-SKT-HINTNY          PIC       X(01).
     03  MD02-SKT-HINTN3          PIC       X(01).
     03  MD02-SKT-HINTN4          PIC       X(01).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  MD02-KIKAKU              PIC       X(15).
     03  FILLER                   PIC       X(47)  VALUE  SPACE.
     03  MD02-RICD                PIC       9(02).
     03  MD02-RICD1               PIC       X(01).
     03  MD02-RICDNM              PIC       N(06)
                                  CHARACTER   TYPE IS   PITCH-15.
*
***** 合計行
 01  GK01             CHARACTER      TYPE IS     PITCH-15.
     03  FILLER                   PIC       X(77)  VALUE  SPACE.
     03  GK01-TAITOL              PIC       N(06).
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GK01-SURYO               PIC       -,---,--9.9.
     03  FILLER                   PIC       X(01)  VALUE  SPACE.
     03  GK01-GENKA               PIC       -,---,--9.99.
     03  GK01-KINGAKU             PIC       ----,---,--9.
     03  FILLER                   PIC       X(180) VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-STYMD             PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-STYMD.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE           NFJYURF.
     MOVE     "NFJYURF"           TO        ERR-FL-ID.
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
 FILEERROR-SEC3         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFUNMAF.
     MOVE     "NFUNMAF "          TO        ERR-FL-ID.
     MOVE     UNM-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC4         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFMEIMS.
     MOVE     "NFMEIMS "          TO        ERR-FL-ID.
     MOVE     MEI-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY3768N-START         SECTION.
*
     MOVE   "SSY3768N-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
*
     IF    END-FLG    NOT =  "END"
           PERFORM    MAIN-SEC  UNTIL     END-FLG   =  "END"
     END-IF.
*****PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
*
     PERFORM            END-SEC.
*
     STOP               RUN.
*
 SSY3768N-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFUNMAF.
     OPEN     INPUT     NFJYURF.
     OPEN     INPUT     TENMS1.
     OPEN     INPUT     NFMEIMS.
     OPEN     OUTPUT    PRINTF.
*
     MOVE     ZERO           TO    WK-DENKEI
                                   WK-TENKEI
                                   WK-SOKEI.
     INITIALIZE                    WK-DENKEI
                                   WK-TENKEI
                                   WK-SOKEI.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     99             TO    L-CNT.
     MOVE     1              TO    PRT-FLG.
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
     MOVE  SPACE                TO   JYU-REC
     INITIALIZE                      JYU-REC
     MOVE  PARA-STYMD           TO   JYU-F01
*
     START  NFJYURF  KEY  >=   JYU-F01  JYU-F05
                               JYU-F07  JYU-F08
         INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データ無し１＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START
*
     PERFORM  NFJYURF-RD-SEC
     IF    END-FLG   =   "END"
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
*
*　ブレイクキー設定
     MOVE  JYU-F05    TO       BRK-TENCD.
     MOVE  JYU-F07    TO       BRK-DENCD.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　受領累積ファイル読み込み　　　
****************************************************************
 NFJYURF-RD-SEC            SECTION.
*
     MOVE    "NFJYURF-RD-SEC"    TO   S-NAME.
*
     READ     NFJYURF
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFJYURF-RD-EXIT
     END-READ.
*指定された受信日を出力する。
     IF   JYU-F01   =   PARA-STYMD
          CONTINUE
     ELSE
          MOVE     "END"      TO   END-FLG
          GO     TO    NFJYURF-RD-EXIT
     END-IF.
*
 NFJYURF-RD-EXIT.
     EXIT.
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*
     MOVE    SPACE                TO    MD01  MD02.
*  店舗ＣＤがブレイク時
     IF  JYU-F05  NOT =  BRK-TENCD
*        店舗合計
         PERFORM   TENKEI-SEC
         MOVE  JYU-F05    TO       BRK-TENCD
         MOVE  1          TO       PRT-FLG
     END-IF.
*  改頁チェック
     IF       L-CNT     >    58
              PERFORM  MIDASI-SEC
     END-IF.
*  明細行編集出力
     IF       PRT-FLG       =   "1"
       MOVE     JYU-F05           TO    MD01-TENCD
       PERFROM  TENMS1-READ-SEC
       IF   TENMS1-INV-FLG = "INV"
            MOVE NC"店舗未登録"   TO    MD01-TENMEI
       ELSE
            MOVE TEN-F03          TO    MD01-TENMEI
       END-IF
       MOVE     SPACE             TO    PRT-FLG
     END-IF.
*  計上日
     MOVE     JYU-F02              TO        MD01-KEIJYOBI.
*  伝区
     MOVE     JYU-F03              TO        MD01-DENKU.
     MOVE     ":"                  TO        MD01-DENKU1.
*  伝区名称
     EVALUATE   JYU-F03
         WHEN   "01"
                MOVE     NC"仕入"  TO    MD01-DNKNM
         WHEN   "02"
                MOVE     NC"仕入"  TO    MD01-DNKNM
         WHEN   "11"
                MOVE     NC"返品"  TO    MD01-DNKNM
         WHEN   "12"
                MOVE     NC"値引"  TO    MD01-DNKNM
         WHEN   "61"
                MOVE     NC"相殺"  TO    MD01-DNKNM
     END-EVALUATE.
*  赤黒
     MOVE     JYU-F04              TO        MD01-AKAKURO.
     MOVE     ":"                  TO        MD01-AKAKURO1.
     EVALUATE   JYU-F04
         WHEN   0
                MOVE     NC"黒"    TO    MD01-DNKNM
         WHEN   1
                MOVE     NC"赤"    TO    MD01-DNKNM
         WHEN   OTHER
                MOVE     NC"？"    TO    MD01-DNKNM
     END-EVALUATE.
*  伝票番号
     MOVE     JYU-F07              TO        MD01-DENNO.
*  行番
     MOVE     JYU-F08              TO        MD01-GYO.
*  商品ＣＤ　
     MOVE     JYU-F10              TO        MD01-SHOCD.
*  商品名
     MOVE     JYU-F10              TO        MEI-F01.
     PERFORM  NFSHOMS-READ-SEC.
     IF  NFSHOMS-INV-FLG  = "INV"
         MOVE  ALL "*"             TO        MD01-SHONM
         MOVE  ALL "*"             TO        MD02-KIKAKU
     ELSE
         MOVE  MEI-F07             TO        MD01-SHONM
         MOVE  MEI-F08             TO        MD02-KIKAKU
     END-READ.
*  受領数　
     MOVE     JYU-F12              TO        WK-SURYO.
     COMPUTE  WK-SURYOR   =   WK-SURYO  /  10.
     MOVE     WK-SURYOR            TO        MD01-JYURYOSU.
*  原価単価
     MOVE     JYU-F13              TO        WK-GENKA.
     COMPUTE  WK-GENKAR   =   WK-GENKA  /  100.
     MOVE     WK-GENKAR            TO        MD01-GENKA.
*  原価金額
     COMPUTE  WK-KINGAKU  =    WK-SURYO  *   WK-GENKA.
     MOVE     WK-KINGAKU           TO        MD01-KINGAKU.
*  受領区分
     MOVE     JYU-F15              TO        MD01-JYU.
*  理由区分
     MOVE     JYU-F16              TO        MD01-RIYU.
*  理由ＣＤ
     MOVE     JYU-F17              TO        MD01-RICD.
*--------------
*  伝票計加算
*--------------
*  受領数　
     COMPUTE  WK-SURYO-DEN    =   WK-SURYO-DEN     +  WK-SURYOR.
*  原価単価
     COMPUTE  WK-GENKA-DEN    =   WK-GENKA-DEN     +  WK-GENKAR.
*  原価金額
     COMPUTE  WK-KINGAKU-DEN  =   WK-KINGAKU-DEN   +  WK-KINGAKU.
*
*--------------
*  店舗計加算
*--------------
*  受領数　
     COMPUTE  WK-SURYO-TEN    =   WK-SURYO-TEN     +  WK-SURYOR.
*  原価単価
     COMPUTE  WK-GENKA-TEN    =   WK-GENKA-TEN     +  WK-GENKAR.
*  原価金額
     COMPUTE  WK-KINGAKU-TEN  =   WK-KINGAKU-TEN   +  WK-KINGAKU.
*
*--------------
*  総合計加算
*--------------
*  受領数　
     COMPUTE  WK-SURYO-KEI    =   WK-SURYO-KEI     +  WK-SURYOR.
*  原価単価
     COMPUTE  WK-GENKA-KEI    =   WK-GENKA-KEI     +  WK-GENKAR.
*  原価金額
     COMPUTE  WK-KINGAKU-KEI  =   WK-KINGAKU-KEI   +  WK-KINGAKU.
*
*--------------
*  明細行出力
*--------------
     WRITE  P-REC  FROM  MD01  AFTER 1.
     ADD      1              TO   L-CNT.
*
*    次レコード読込み
     PERFORM  NFJYURF-RD-SEC.
*
 MAIN-EXIT.
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
     MOVE     SYS-YY            TO   HD01-YY.
     MOVE     SYS-MM            TO   HD01-MM.
     MOVE     SYS-DD            TO   HD01-DD.
*
     MOVE     PARA-STYMD        TO   HD02-STYMD.
     MOVE     PARA-EDYMD        TO   HD02-EDYMD.
*
*頁セット
     ADD      1              TO   P-CNT.
     MOVE     P-CNT          TO   HD01-PCNT.
*ヘッダー出力
     WRITE    P-REC     FROM      HD01      AFTER     1.
     WRITE    P-REC     FROM      HD02      AFTER     2.
     WRITE    P-REC     FROM      HD03      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
     WRITE    P-REC     FROM      HD04      AFTER     1.
     WRITE    P-REC     FROM      SEN1      AFTER     1.
*
     MOVE     8         TO        L-CNT.
*
 MIDASI-EXIT.
     EXIT.
****************************************************************
*             伝票合計出力　                　　
****************************************************************
 DENKEI-SEC             SECTION.
*
     MOVE    "DENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜伝票合計＞"   TO   GK01-TAITOL.
*  受領数　
     MOVE     WK-SURYO-DEN       TO   GK01-SURYO.
*  原価単価
     MOVE     WK-GENKA-DEN       TO   GK01-GENKA.
*  原価金額
     MOVE     WK-KINGAKU-DEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
     WRITE    P-REC     FROM  SEN2  AFTER 1.
     ADD      1         TO    L-CNT.
*
 DENKEI-EXIT.
     EXIT.
*
****************************************************************
*             店舗合計出力　                　　
****************************************************************
 TENKEI-SEC             SECTION.
*
     MOVE    "TENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜店舗合計＞"   TO   GK01-TAITOL.
*  受領数　
     MOVE     WK-SURYO-TEN       TO   GK01-SURYO.
*  原価単価
     MOVE     WK-GENKA-TEN       TO   GK01-GENKA.
*  原価金額
     MOVE     WK-KINGAKU-TEN     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
     WRITE    P-REC     FROM  SEN2  AFTER 1.
     ADD      1         TO    L-CNT.
*
 TENKEI-EXIT.
     EXIT.
*
****************************************************************
*             総合計出力　　                　　
****************************************************************
 SOUGOKEI-SEC           SECTION.
*
     MOVE    "TENKEI-SEC"        TO   S-NAME.
*改頁
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
*
*--------------
*  合計転送
*--------------
*  タイトル転送
     MOVE     NC"＜総合計　＞"   TO   GK01-TAITOL.
*  受領数　
     MOVE     WK-SURYO-KEI       TO   GK01-SURYO.
*  原価単価
     MOVE     WK-GENKA-KEI       TO   GK01-GENKA.
*  原価金額
     MOVE     WK-KINGAKU-KEI     TO   GK01-KINGAKU.
*
*伝票合計出力
     WRITE    P-REC     FROM      GK01      AFTER     1.
*
     ADD      1         TO        L-CNT.
*
*  線出力
     IF       L-CNT  >=  58
              PERFORM  MIDASI-SEC
     END-IF.
     WRITE    P-REC     FROM  SEN2  AFTER 1.
     ADD      1         TO    L-CNT.
*
 SOUGOKEI-EXIT.
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
*             ナフコ商品マスタ読込
***************************************************************
 NFSHOMS-READ-SEC       SECTION.
*
     MOVE    "NFSHOMS-READ-SEC" TO        S-NAME.
*
     READ     NFSHOMS
              INVALID      MOVE  "INV"    TO   NFSHOMS-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-READ-EXIT.
     EXIT.
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     IF      L-CNT   NOT =  99
* 商品合計出力
             PERFORM   DENKEI-SEC
* 商品合計出力
             PERFORM   TENKEI-SEC
* 総合計出力
             PERFORM   SOUGOKEI-SEC
     END-IF.
*
     DISPLAY  MSG-END   UPON CONS.
*
     CLOSE    NFJYURF  TENMS1  NFMEIMS  PRINTF  NFUNMAF.
*
 END-EXIT.
     EXIT.
*

```
