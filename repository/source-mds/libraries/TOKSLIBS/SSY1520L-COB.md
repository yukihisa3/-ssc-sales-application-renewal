# SSY1520L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY1520L.COB`

## ソースコード

```cobol
**********************************************************
*                                                        *
*      セキチューオンライン検収書（物品受領書）発行      *
*                              サカタのタネ殿向          *
**********************************************************
**********************************************************
 IDENTIFICATION            DIVISION.
**********************************************************
 PROGRAM-ID.               SSY1520L.
 AUTHOR.                   OONO.
 DATE-WRITTEN.             10/04/21.
**********************************************************
 ENVIRONMENT               DIVISION.
**********************************************************
 CONFIGURATION             SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     YA             IS     PCHI20
     YB             IS     PCHI1
     YB-21          IS     PCHI15
     STATION        IS     STA
     CONSOLE        IS     CONSL.
******************************************************************
*             INPUT-OUTPUT                                       *
******************************************************************
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*%%%%%%%%%%%%%%%%%%%%%* 検収データファイル *%%%%%%%%%%%%%%%%%%%%
     SELECT   SKENSYUD   ASSIGN    TO        DA-01-S-SKKENSYU
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    SKEN-ST  SKEN-ST1.
*%%%%%%%%%%%%%%%%%%%%%%%* プリンター *%%%%%%%%%%%%%%%%%%%%%%%%*
     SELECT   PRTFILE   ASSIGN    TO        LP-04-PRTF.
*%%%%%%%%%%%%%%%%%%%%%%%* 表示ファイル *%%%%%%%%%%%%%%%%%%%%%%*
     SELECT   DSPF      ASSIGN    TO        01-GS-DSPF
                        ORGANIZATION        SEQUENTIAL
                        DESTINATION    IS  "DSP"
                        FORMAT         IS   DSP-FORM
                        GROUP          IS   DSP-GRP
                        PROCESSING     IS   DSP-PROC
                        FUNCTION       IS   PF-KEY
                        CONTROL        IS   DSP-CONTROL
                        STATUS         IS   DSP-ST   DSP-ST1.
******************************************************************
*             DATA                DIVISION                       *
******************************************************************
 DATA                      DIVISION.
 FILE                      SECTION.
*----------------------------------------------------------------*
*            検収データファイル      RL=300  BF=3
*----------------------------------------------------------------*
 FD  SKENSYUD
     BLOCK       CONTAINS  3         RECORDS
     LABEL       RECORD    IS        STANDARD.
 01  SKEN-REC.
     03  SKEN01            PIC X(01).
     03  SKEN02            PIC X(02).
     03  SKEN03            PIC 9(09).
     03  SKEN04            PIC 9(04).
     03  SKEN05            PIC 9(05).
     03  SKEN06            PIC 9(04).
     03  SKEN07            PIC X(02).
     03  SKEN08            PIC X(01).
     03  SKEN09.
         05  SKEN09Y       PIC 9(02).
         05  SKEN09M       PIC 9(02).
         05  SKEN09D       PIC 9(02).
     03  SKEN10.
         05  SKEN10Y       PIC 9(02).
         05  SKEN10M       PIC 9(02).
         05  SKEN10D       PIC 9(02).
     03  SKEN11.
         05  SKEN11Y       PIC 9(02).
         05  SKEN11M       PIC 9(02).
         05  SKEN11D       PIC 9(02).
     03  SKEN12            PIC X(06).
     03  SKEN13            PIC X(02).
     03  SKEN14            PIC X(15).
     03  SKEN15            PIC X(15).
     03  SKEN16            PIC 9(06).
     03  SKEN17            PIC 9(09).
     03  SKEN18            PIC X(15).
     03  SKEN19            PIC X(14).
     03  SKEN20            PIC X(01).
     03  SKEN21            PIC X(02).
     03  SKEN22            PIC 9(02).
     03  SKEN23            PIC X(13).
     03  SKEN24            PIC X(04).
     03  SKEN25            PIC X(04).
     03  SKEN26            PIC X(02).
     03  SKEN27            PIC 9(05)V9.
     03  SKEN28            PIC 9(07)V99.
     03  SKEN29            PIC 9(07).
     03  SKEN30            PIC 9(10).
     03  SKEN31            PIC 9(10).
     03  SKEN32            PIC X(08).
     03  SKEN33            PIC X(20).
     03  SKEN34            PIC X(20).
     03  SKEN35            PIC X(02).
     03  SKEN36            PIC X(01).
     03  SKEN37            PIC X(01).
     03  SKEN38            PIC 9(06).
     03  FILLER            PIC X(34).
     03  SKEN98            PIC 9(05).
     03  SKEN99            PIC 9(05).
***プリンタ***
 FD  PRTFILE
     LABEL       RECORD    IS        OMITTED
     LINAGE                IS        33.
 01  PRTF-REC.
    03  FILLER             PIC  X(200).
***表示ファイル***
 FD  DSPF
     LABEL       RECORD    IS        OMITTED.
     COPY        FSY15201   OF        XMDLIB.
******************************************************************
 WORKING-STORAGE       SECTION.
******************************************************************
*%%%%%*表示パラメータ*%%%%%*
 01  FORM-PARA.
     03  DSP-FORM                 PIC  X(08).
     03  DSP-PROC                 PIC  X(02).
     03  DSP-GRP                  PIC  X(08).
     03  PF-KEY                   PIC  X(04).
     03  DSP-CONTROL.
         05  DSP-CNTRL            PIC  X(04).
         05  DSP-STR-PG           PIC  X(02).
*
 01  FILE-STATUS.
     03  DSP-STATUS               PIC  X(02).
     03  SKEN-ST                  PIC  X(02).
     03  SKEN-ST1                 PIC  X(04).
     03  DSP-ST                   PIC  X(02).
     03  DSP-ST1                  PIC  X(04).
*%%%%%* 合計計算WORK REC *%%%%%*
 01  WK-KEISAN.
     03  SUU-KEI               PIC  9(05)  VALUE  ZERO.
     03  GEN-KEI               PIC  9(09)  VALUE  ZERO.
     03  URI-KEI               PIC  9(09)  VALUE  ZERO.
*%%%%%* 帳票編集WORK REC *%%%%%*
 01  WK-LIST.
****  ｹﾞﾝﾀﾝｶ
     03  WK-GENTAN                PIC  9(07)V99.
     03  WK-GENTANR               REDEFINES WK-GENTAN.
         05  WK-GENTANA           PIC  9(07).
         05  WK-GENTANB           PIC  9(02).
*
 01  WDSP-REC.
     03  WDSP-KAISI               PIC  9(09) VALUE     ZERO.
     03  WDSP-END                 PIC  9(09) VALUE     ZERO.
 01  WK-AREA.
     03  WK-KOTAE                 PIC  9(05) VALUE     ZERO.
     03  WK-AMARI                 PIC  9(05) VALUE     ZERO.
     03  END-SW                   PIC  9(01) VALUE     ZERO.
     03  ERR-SW                   PIC  9(01) VALUE     ZERO.
     03  OK-SW                    PIC  9(01) VALUE     ZERO.
     03  MSG-SW                   PIC  9(01) VALUE     ZERO.
     03  LOOP-CNT                 PIC  9(02) VALUE     ZERO.
     03  DUMMY-CNT                PIC  9(02) VALUE     ZERO.
     03  WK-MAISUU                PIC  9(05) VALUE     ZERO.
     03  PAGE-CNT                 PIC  9(05) VALUE     ZERO.
*    03  WK-SYSDATE               PIC  9(06) VALUE     ZERO.
*    03  WK-SYSDATER    REDEFINES WK-SYSDATE.
*        05  WK-SYSYY             PIC 9(02).
*        05  WK-SYSMM             PIC 9(02).
*        05  WK-SYSDD             PIC 9(02).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
 01  IN-DATA                      PIC  X(01).
 01  FILE-ERR.
     03  FILE-ERR1                PIC  N(10) VALUE
              NC"検収データエラー　".
 01  FILE-ERR.
     03  FILE-ERR2                PIC  N(10) VALUE
              NC"画面ファイルエラー　".
 01  WK-ERR.
     03  WK-ERR1                  PIC  N(12) VALUE
              NC"　　開始伝票_より小さい".
     03  WK-ERR2                  PIC  N(12) VALUE
              NC"　　　　『テスト印字中』".
     03  WK-ERR3                  PIC  N(12) VALUE
              NC"　　　　『検収書作成中』".
     03  WK-ERR4                  PIC  N(12) VALUE
              NC"　　対象伝票ナンバー無し".
 01  WK-MSG.
     03  FILLER                   PIC  X(19) VALUE
              "ﾃﾞﾝﾋﾟｮｳ ﾊｯｺｳﾏｲｽｳ = ".
     03  DSP-PAGE                 PIC  9(05) VALUE      ZERO.
*発注数量変換
 01  WK-LHATYU             PIC 9(05)V9  VALUE  ZERO.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*%%%%%%%%%%%%%%%%%%%%%%%%%* ＬＩＳＴ *%%%%%%%%%%%%%%%%%%%%%%%%*
*%%* 見出し部 *%%*
 01  LIST-M2.
     02  M201      CHARACTER      TYPE  IS  PCHI15.
     03  FILLER                   PIC  X(16)  VALUE  SPACE.
     03  FILLER                   PIC  N(25)  VALUE
         NC"＊＊　_セキチュー　検収書（物品受領書）植物　＊＊".
     03  FILLER                   PIC  X(10)  VALUE  SPACE.
     03  FILLER                   PIC  X(05)  VALUE  "DATE:".
     03  LSYSYY                   PIC  Z9     VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ".".
     03  LSYSMM                   PIC  Z9     VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ".".
     03  LSYSDD                   PIC  Z9     VALUE  ZERO.
     03  FILLER                   PIC  X(05)  VALUE  "  NO:".
     03  LPAGE                    PIC  ZZZZ9  VALUE  ZERO.
 01  LIST-M3.
     02  M301      CHARACTER      TYPE  IS  PCHI20.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"社名：".
     03  FILLER                   PIC  X(09)  VALUE "(KK)ｾｷﾁｭｰ".
     03  FILLER                   PIC  X(08)  VALUE  SPACE.
     03  FILLER                   PIC  N(05)  VALUE
                                              NC"伝票区分：".
     03  LDENKU                   PIC  N(04)  VALUE  SPACE.
     03  FILLER                   PIC  X(25)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"取引先".
     03  FILLER                   PIC  X(02)  VALUE "CD".
     03  FILLER                   PIC  N(01)  VALUE  NC"：".
     03  LTORCD                   PIC  X(06)  VALUE  SPACE.
     03  FILLER                   PIC  X(19)  VALUE  SPACE.
     03  FILLER                   PIC  N(07)  VALUE
                                       NC"訂正元伝票_：".
     03  LTEINO                   PIC  9(09)  VALUE  ZERO.
 01  LIST-M4.
     02  M401      CHARACTER      TYPE  IS  PCHI20.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"店名：".
     03  LTENNM                   PIC  X(15)  VALUE  SPACE.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  FILLER                   PIC  N(02)  VALUE  NC"店：".
     03  LTENCD                   PIC  9(05)  VALUE  ZERO.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"部門：".
     03  LBUNCD                   PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"伝票_：".
     03  LDENNO                   PIC  9(09)  VALUE  ZERO.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(05)  VALUE
                                              NC"取引先名：".
     03  LTORNM1                  PIC  X(20)  VALUE  SPACE.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  X(02)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"検収日：".
     03  LNOUYY                   PIC  Z9     VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ".".
     03  LNOUMM                   PIC  Z9     VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ".".
     03  LNOUDD                   PIC  Z9     VALUE  ZERO.
 01  LIST-M5.
     02  M501      CHARACTER      TYPE  IS  PCHI20.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(06)  VALUE
                                       NC"商品名／規格".
     03  FILLER                   PIC  X(12)  VALUE  SPACE.
     03  FILLER                   PIC  N(06)  VALUE
                                       NC"商品／ＪＡＮ".
     03  FILLER                   PIC  X(04)  VALUE "ｺｰﾄﾞ".
     03  FILLER                   PIC  X(11)  VALUE  SPACE.
     03  FILLER                   PIC  N(02)  VALUE  NC"数量".
     03  FILLER                   PIC  X(15)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"原単価".
     03  FILLER                   PIC  X(05)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"原価金額".
     03  FILLER                   PIC  X(05)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE  NC"売単価".
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"売価金額".
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"　備考　".
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"返品理由".
*%%* 明細部 *%%*
 01  LIST-D1.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  LSHONM                   PIC  X(20)  VALUE  SPACE.
     03  FILLER                   PIC  X(08)  VALUE  SPACE.
     03  LSHOCD                   PIC  X(08)  VALUE  SPACE.
 01  LIST-D2.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  LKIKAKU                  PIC  X(20)  VALUE  SPACE.
     03  FILLER                   PIC  X(04)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE "(".
     03  LJANCD                   PIC  X(13)  VALUE  SPACE.
     03  FILLER                   PIC  X(01)  VALUE ")".
     03  FILLER                   PIC  X(10)  VALUE  SPACE.
     03  LSURYO                   PIC ZZ,ZZ9   VALUE  ZERO.
     03  FILLER                   PIC  X(01)   VALUE  SPACE.
     03  LKIGOU-1                 PIC  X(01)   VALUE  SPACE.
     03  LHATYU                   PIC  ZZZ,ZZZ.
     03  LKIGOU-2                 PIC  X(01)   VALUE  SPACE.
     03  FILLER                   PIC  X(04)   VALUE  SPACE.
     03  LGENTAN1                 PIC ZZZ,ZZ9  VALUE  ZERO.
     03  LGENTAN2                 PIC ZZ       VALUE  ZERO.
     03  LGENKIN                  PIC ZZZ,ZZZ,ZZ9  VALUE  ZERO.
     03  FILLER                   PIC  X(04)   VALUE  SPACE.
     03  LURITAN                  PIC ZZZ,ZZ9  VALUE  ZERO.
     03  LURIKIN                  PIC ZZZ,ZZZ,ZZ9  VALUE  ZERO.
     03  D201      CHARACTER      TYPE  IS  PCHI20.
         05  FILLER               PIC  X(01)   VALUE  SPACE.
         05  LBIKOU               PIC  N(04)   VALUE  SPACE.
         05  FILLER               PIC  X(01)   VALUE  SPACE.
         05  LHENPIN              PIC  N(06)   VALUE  SPACE.
*%%* 合計部 *%%*
 01  LIST-G.
     02  G01       CHARACTER      TYPE  IS  PCHI20.
     03  FILLER                   PIC  X(46)  VALUE  SPACE.
     03  FILLER                   PIC  N(03)  VALUE
                                              NC"数量計".
     03  LSUU-KEI                 PIC  ZZ,ZZ9  VALUE  ZERO.
     03  FILLER                   PIC  X(15)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"原価合計".
     03  LGEN-KEI                 PIC  ZZZ,ZZZ,ZZ9  VALUE  SPACE.
     03  FILLER                   PIC  X(03)  VALUE  SPACE.
     03  FILLER                   PIC  N(04)  VALUE
                                              NC"売価合計".
     03  LURI-KEI                 PIC  ZZZ,ZZZ,ZZ9  VALUE  SPACE.
*%%* 合計部 *%%*
 01  LIST-SEN.
     03  LSEN                     PIC  X(200) VALUE  SPACE.
*%%* 空白部 *%%*
 01  DUMMY.
     03  FILLER                   PIC  X(01)  VALUE  SPACE.
******************************************************************
*             PROCEDURE           DIVISION                       *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 000-SKEN-ERR           SECTION.
     USE  AFTER    EXCEPTION PROCEDURE SKENSYUD.
     DISPLAY       FILE-ERR1 UPON      STA.
     DISPLAY       SKEN-ST   UPON      STA.
     DISPLAY       SKEN-ST1  UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP  RUN.
 000-DSP-ERR            SECTION.
     USE  AFTER    EXCEPTION PROCEDURE DSPF.
     DISPLAY       FILE-ERR2 UPON      STA.
     DISPLAY       DSP-ST    UPON      STA.
     DISPLAY       DSP-ST1   UPON      STA.
     MOVE          4000      TO        PROGRAM-STATUS.
     STOP  RUN.
 END DECLARATIVES.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 PROC-SEC               SECTION.
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC       UNTIL     END-SW = 9.
     PERFORM       END-SEC.
     STOP  RUN.
 PROC-EXIT.
     EXIT.
**********************************************************
*                      Ｉ Ｎ Ｉ Ｔ                       *
**********************************************************
 INIT-SEC               SECTION.
     OPEN     INPUT     SKENSYUD.
     OPEN     OUTPUT    PRTFILE.
     OPEN     I-O       DSPF.
*
     INITIALIZE         WK-LIST.
     INITIALIZE         LOOP-CNT  WK-KEISAN.
*    ACCEPT   WK-SYSDATE    FROM  DATE.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*画面表示日付編集
     MOVE      SYS-DATE(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*
     READ     SKENSYUD   AT        END
              MOVE      9         TO        END-SW
              GO        TO        INIT-EXIT
     END-READ.
     MOVE     SKEN99    TO        WK-MAISUU.
     CLOSE    SKENSYUD.
     OPEN     INPUT     SKENSYUD.
     MOVE     ZERO      TO        END-SW.
 INIT-EXIT.
     EXIT.
***********************************************************
*                     ＭＡＩＮ処理                        *
***********************************************************
 MAIN-SEC               SECTION.
     PERFORM            DSP-SEC.
     IF   ( END-SW = 9 )
              GO        TO        MAIN-EXIT
     END-IF.
 MAIN-010.
*検収データ初期読み*
     READ     SKENSYUD   AT        END
              MOVE      9         TO        END-SW
              GO        TO        MAIN-050
     END-READ.
     IF   ( SKEN03  >  WDSP-END )
          IF   ( R040 = 3 )
              CLOSE   SKENSYUD
              OPEN    INPUT       SKENSYUD
              MOVE    ZERO        TO        END-SW
              GO                  TO        MAIN-SEC
          ELSE
              MOVE    9           TO        END-SW
              GO                  TO        MAIN-EXIT
          END-IF
     END-IF.
     IF   ( SKEN03  <  WDSP-KAISI )
              GO                  TO        MAIN-010
     END-IF.
     MOVE     SKEN03              TO        WDSP-KAISI.
     MOVE     WK-ERR3             TO        W050.
     MOVE    "W050"               TO        DSP-GRP.
     PERFORM  DSP-WRT-SEC.
 MAIN-020.
*見出し印刷
     INITIALIZE         LOOP-CNT  WK-KEISAN.
     PERFORM  MIDA-EDT-SEC.
     PERFORM  MIDA-WRT-SEC.
 MAIN-030.
*明細印刷
     ADD      1                   TO        LOOP-CNT.
     PERFORM  MEI-EDT-SEC.
     PERFORM  MEI-WRT-SEC.
 MAIN-035.
*検収データＲＥＡＤ*
     READ     SKENSYUD   AT        END
              MOVE      9         TO        END-SW
              GO        TO        MAIN-040
     END-READ.
     IF    ( SKEN03  >  WDSP-END )
              MOVE      9         TO        END-SW
              GO                  TO        MAIN-040
     END-IF.
     IF    ( SKEN03  NOT =  WDSP-KAISI )
              MOVE      SKEN03    TO        WDSP-KAISI
     ELSE
              GO                  TO        MAIN-030
     END-IF.
 MAIN-040.
*合計印刷
     PERFORM  DUMMY-WRT-SEC.
     PERFORM  COMP-EDT-SEC.
     PERFORM  COMP-WRT-SEC.
     PERFORM  COMP-SEN-SEC.
     PERFORM  KAI-SEC.
     IF    (  END-SW  NOT =  9 )
              GO                  TO        MAIN-020
     ELSE
*    奇数頁出力時ストックホーム制御処理
        IF       PAGE-CNT  NOT =  ZERO
                 DIVIDE   PAGE-CNT  BY  2  GIVING    WK-KOTAE
                                           REMAINDER WK-AMARI
                 IF       WK-AMARI  =  ZERO
                          MOVE   SPACE   TO     PRTF-REC
                          WRITE  PRTF-REC AFTER  PAGE
                 END-IF
        END-IF
     END-IF.
 MAIN-050.
     IF    (  END-SW  =  9 ) AND (R040 = 3)
              CLOSE   SKENSYUD
              OPEN    INPUT       SKENSYUD
              MOVE    ZERO        TO        END-SW
              GO                  TO        MAIN-SEC
     END-IF.
 MAIN-EXIT.
     EXIT.
**********************************************************
*                       Ｅ Ｎ Ｄ                         *
**********************************************************
 END-SEC                SECTION.
     CLOSE    SKENSYUD   PRTFILE   DSPF.
***  MOVE     PAGE-CNT  TO        DSP-PAGE.
***  DISPLAY  WK-MSG    UPON      STA.
***  ACCEPT   IN-DATA   FROM      STA.
 END-EXIT.
     EXIT.
**********************************************************
*                    明細データ編集                      *
**********************************************************
 MEI-EDT-SEC               SECTION.
     MOVE        SKEN33      TO        LSHONM.
     MOVE        SKEN32      TO        LSHOCD.
     MOVE        SKEN34      TO        LKIKAKU.
     MOVE        SKEN23      TO        LJANCD.
     MOVE        SKEN27      TO        LSURYO.
     MOVE        SKEN28      TO        WK-GENTAN.
     MOVE        WK-GENTANA  TO        LGENTAN1.
     MOVE        WK-GENTANB  TO        LGENTAN2.
     MOVE        SKEN29      TO        LURITAN.
     MOVE        SKEN30      TO        LGENKIN.
     MOVE        SKEN31      TO        LURIKIN.
*備考（数量訂正有無より）
     EVALUATE    SKEN36
         WHEN   "0"
                 MOVE    SPACE           TO  LBIKOU
         WHEN   "1"
                 MOVE    NC"数量訂正"    TO  LBIKOU
         WHEN    OTHER
                 MOVE    SPACE           TO  LBIKOU
     END-EVALUATE.
*返品理由（返品理由より）
     EVALUATE    SKEN35
         WHEN   "01"
                 MOVE    NC"納品過多　　"  TO  LHENPIN
         WHEN   "02"
                 MOVE    NC"汚破損　　　"  TO  LHENPIN
         WHEN   "03"
                 MOVE    NC"発注外　　　"  TO  LHENPIN
         WHEN   "04"
                 MOVE    NC"未納品　　　"  TO  LHENPIN
         WHEN   "11"
                 MOVE    NC"商品入替え　"  TO  LHENPIN
         WHEN   "12"
                 MOVE    NC"販売期間終了"  TO  LHENPIN
         WHEN   "13"
                 MOVE    NC"不良品　　"    TO  LHENPIN
         WHEN   "14"
                 MOVE    NC"バイヤー指示"  TO  LHENPIN
         WHEN   "91"
                 MOVE    NC"その他商談"    TO  LHENPIN
         WHEN    OTHER
                 MOVE    SPACE             TO  LHENPIN
     END-EVALUATE.
* 2002/09/19 NAV 追加 START *
     MOVE        "("         TO        LKIGOU-1.
     COMPUTE  WK-LHATYU    =    SKEN38  /  10.
     MOVE     WK-LHATYU      TO        LHATYU.
     MOVE        ")"         TO        LKIGOU-2.
* 2002/09/19 NAV 追加 START *
*
     COMPUTE     SUU-KEI   =  SUU-KEI + SKEN27.
     COMPUTE     GEN-KEI   =  GEN-KEI + SKEN30.
     COMPUTE     URI-KEI   =  URI-KEI + SKEN31.
 MEI-EDT-EXIT.
     EXIT.
*========================================================*
*                     明細出力処理                       *
*========================================================*
 MEI-WRT-SEC            SECTION.
     WRITE    PRTF-REC  FROM      LIST-D1   AFTER     1.
     WRITE    PRTF-REC  FROM      LIST-D2   AFTER     1.
 MEI-WRT-EXIT.
     EXIT.
**********************************************************
*                      合計欄編集                        *
**********************************************************
 COMP-EDT-SEC              SECTION.
     MOVE        SUU-KEI   TO   LSUU-KEI.
     MOVE        GEN-KEI   TO   LGEN-KEI.
     MOVE        URI-KEI   TO   LURI-KEI.
 COMP-EDT-EXIT.
     EXIT.
*========================================================*
*                      合計出力処理                      *
*========================================================*
 COMP-WRT-SEC           SECTION.
     WRITE    PRTF-REC  FROM      LIST-G    AFTER     2.
 COMP-WRT-EXIT.
     EXIT.
*========================================================*
*                       線打ち                           *
*========================================================*
 COMP-SEN-SEC          SECTION.
     MOVE     ALL "-"      TO    LSEN.
     WRITE    PRTF-REC  FROM      LIST-SEN  AFTER     2.
 COMP-SEN-EXIT.
     EXIT.
**********************************************************
*                 見出しデータ編集処理                   *
**********************************************************
 MIDA-EDT-SEC              SECTION.
     ADD         1         TO        PAGE-CNT.
*
     MOVE        PAGE-CNT  TO        LPAGE.
     MOVE        WK-Y      TO        LSYSYY.
     MOVE        WK-M      TO        LSYSMM.
     MOVE        WK-D      TO        LSYSDD.
*伝票区分（伝票区分より）
     EVALUATE    SKEN07
         WHEN   "01"
                 MOVE    NC"定番発注"    TO  LDENKU
         WHEN   "02"
                 MOVE    NC"特売発注"    TO  LDENKU
         WHEN   "03"
                 MOVE    NC"スポ発注"    TO  LDENKU
         WHEN   "04"
                 MOVE    NC"客注発注"    TO  LDENKU
         WHEN   "51"
                 MOVE    NC"定番仕入"    TO  LDENKU
         WHEN   "52"
                 MOVE    NC"特売仕入"    TO  LDENKU
         WHEN   "53"
                 MOVE    NC"スポ仕入"    TO  LDENKU
         WHEN   "54"
                 MOVE    NC"客注発注"    TO  LDENKU
         WHEN   "55"
                 MOVE    NC"返品　　"    TO  LDENKU
         WHEN   "56"
                 MOVE    NC"値引き　"    TO  LDENKU
         WHEN   "57"
                 MOVE    NC"リベート"    TO  LDENKU
         WHEN   "91"
                 MOVE    NC"訂正黒　"    TO  LDENKU
         WHEN   "92"
                 MOVE    NC"訂正赤　"    TO  LDENKU
         WHEN    OTHER
                 MOVE    NC"　　　　"    TO  LDENKU
     END-EVALUATE.
     MOVE        SKEN14    TO        LTORNM1.
     MOVE        SKEN15    TO        LTENNM.
     MOVE        SKEN05    TO        LTENCD.
     MOVE        SKEN06    TO        LBUNCD.
     MOVE        SKEN03    TO        LDENNO.
     MOVE        SKEN17    TO        LTEINO.
     MOVE        SKEN12    TO        LTORCD.
     MOVE        SKEN11Y   TO        LNOUYY.
     MOVE        SKEN11M   TO        LNOUMM.
     MOVE        SKEN11D   TO        LNOUDD.
 MIDA-EDT-EXIT.
     EXIT.
*========================================================*
*                   見出し出力処理                       *
*========================================================*
 MIDA-WRT-SEC           SECTION.
     WRITE    PRTF-REC  FROM      LIST-M2   AFTER     2.
     WRITE    PRTF-REC  FROM      LIST-M3   AFTER     2.
     WRITE    PRTF-REC  FROM      LIST-M4   AFTER     1.
     WRITE    PRTF-REC  FROM      DUMMY     AFTER     1.
*
     WRITE    PRTF-REC  FROM      LIST-M5   AFTER     1.
 MIDA-WRT-EXIT.
     EXIT.
**********************************************************
*                       空白行　                         *
**********************************************************
 DUMMY-WRT-SEC          SECTION.
     PERFORM  VARYING  LOOP-CNT  FROM  LOOP-CNT  BY  1
                                 UNTIL LOOP-CNT  >  8
              WRITE    PRTF-REC  FROM  DUMMY     AFTER  2
              END-WRITE
     END-PERFORM.
 DUMMY-WRT-EXIT.
     EXIT.
**********************************************************
*                       改ページ                         *
**********************************************************
 KAI-SEC                SECTION.
     WRITE    PRTF-REC  FROM      DUMMY     AFTER     PAGE.
 KAI-EXIT.
     EXIT.
***********************************************************
*                       画面処理                          *
***********************************************************
 DSP-SEC                SECTION.
     PERFORM       DSP-INT-SEC.
 DSP-010.
     MOVE     "R040"    TO        DSP-GRP.
     PERFORM  DSP-RD-SEC.
     EVALUATE      PF-KEY
        WHEN       "F005"
              MOVE      9         TO        END-SW
              GO                  TO        DSP-EXIT
        WHEN       "E000"
              CONTINUE
        WHEN       OTHER
              GO        TO        DSP-010
     END-EVALUATE.
     IF     (R040  NOT NUMERIC)
             MOVE     ZERO        TO        R040
     END-IF.
******************
* 出力指定　入力 *
******************
     EVALUATE     R040
*テストプリント（未使用）
        WHEN      1
             GO        TO             DSP-010
*全プリント
        WHEN      2
             MOVE      ZERO           TO        WDSP-KAISI
             MOVE      999999999      TO        WDSP-END
                                                R020
             GO        TO             DSP-EXIT
*範囲指定（伝票番号）
        WHEN      3
             CONTINUE
*その他
        WHEN      OTHER
             GO        TO             DSP-010
     END-EVALUATE.
 DSP-020.
     MOVE    "HANI00"   TO        DSP-GRP.
     PERFORM  DSP-RD-SEC.
     EVALUATE       PF-KEY
        WHEN       "F004"
              GO        TO        DSP-SEC
        WHEN       "F005"
              MOVE      9         TO        END-SW
              GO        TO        DSP-EXIT
        WHEN       "E000"
              CONTINUE
        WHEN       OTHER
              GO        TO        DSP-020
     END-EVALUATE.
     IF     (R010  NOT NUMERIC)
             MOVE     ZERO        TO        R010
     END-IF.
     IF      R020  =  ZERO
             MOVE     ALL "9"     TO        WDSP-END  R020
     END-IF.
     IF    ( R010  >  R020 )
             GO        TO        DSP-020
     ELSE
             MOVE      R010      TO        WDSP-KAISI
             MOVE      R020      TO        WDSP-END
             MOVE     "HANI00"   TO        DSP-GRP
             PERFORM   DSP-WRT-SEC
     END-IF.
 DSP-EXIT.
     EXIT.
***********************************************************
*                       画面出力                          *
***********************************************************
 DSP-WRT-SEC               SECTION.
     WRITE   FSY15201.
**   IF  ( DSP-STATUS = "00" )
     IF  ( DSP-ST = "00" )
             MOVE    SPACE     TO        DSP-PROC
     ELSE
         STOP RUN.
 DSP-WRT-EXIT.
     EXIT.
***********************************************************
*                      画面入力                           *
***********************************************************
 DSP-RD-SEC             SECTION.
     MOVE     "NE"      TO        DSP-PROC.
     READ     DSPF      AT        END
                                  STOP      RUN.
     MOVE     SPACE     TO        DSP-PROC.
 DSP-RD-EXIT.
     EXIT.
***********************************************************
*                    画面初期設定                         *
***********************************************************
 DSP-INT-SEC               SECTION.
     MOVE        SPACE     TO        FSY15201.
     INITIALIZE  WDSP-REC  ERR-SW.
     MOVE       "SCREEN"   TO        DSP-GRP.
     MOVE       "FSY15201"  TO        DSP-FORM.
     MOVE       "CL"       TO        DSP-PROC.
     MOVE        HEN-DATE  TO        SDATE.
     MOVE        HEN-TIME  TO        STIME.
     PERFORM     DSP-WRT-SEC.
*ﾏｲｽｳ ﾋｮｳｼﾞ
     MOVE        WK-MAISUU TO        W010.
     MOVE       "W010"     TO        DSP-GRP.
     PERFORM     DSP-WRT-SEC.
 DSP-INT-EXIT.
     EXIT.

```
