# STB0030L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/STB0030L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタ保守　　　　　　　　　　　  *
*    モジュール名　　　　：　商品変換テーブル取込チェックリスト*
*    作成日／作成者　　　：　2017/12/07 INOUE                  *
*    処理概要　　　　　　：　取込チェック結果をリスト出力する。*
*                            （全件・エラー分をＰＡＲＡ制御）　*
*    流用元　　　　　　　：　SSY3931L                          *
*    更新日／更新者　　　：　2018/10/15 NAV TAKAHASHI          *
*    処理概要　　　　　　：　検印欄を追加する。（要望対応）　　*
*    　　　　　　　　　　　　一部、ＬＢＰ出力対応追加　　　　　*
*    更新日／更新者　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            STB0030L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/12/07.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     YA        IS   YA
     YB        IS   YB
     YA-22     IS   YA-22
     YB-22     IS   YB-22
     YB-21     IS   YB-21
     YA-21     IS   YA-21
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*商品変換テーブル取込ファイル3
     SELECT   STBXXXW3  ASSIGN    TO        DA-01-VI-STBXXXW3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SW3-F11  SW3-F13
                                            SW3-F14
                        FILE  STATUS   IS   SW3-STATUS.
*商品変換テーブル取込ファイル2
     SELECT   STBXXXW2  ASSIGN    TO        DA-01-VI-STBXXXW2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SW2-F060 SW2-F11
                                            SW2-F13  SW2-F14
                        FILE  STATUS   IS   SW2-STATUS.
*担当者マスタ
     SELECT     TANMS1       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*---------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*商品変換テーブル取込ファイル1
 FD  STBXXXW3           LABEL RECORD   IS   STANDARD.
     COPY     STBXXXW3  OF        XFDLIB
              JOINING   SW3       PREFIX.
*商品変換テーブル取込ファイル2
 FD  STBXXXW2           LABEL RECORD   IS   STANDARD.
     COPY     STBXXXW2  OF        XFDLIB
              JOINING   SW2       PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  STBXXXW3-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  STBXXXW2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  BK-F11                  PIC  X(01)     VALUE  SPACE.
 01  BRK-MODE                PIC  X(03)     VALUE  SPACE.
*取込日付／時刻バックアップ
 01  WK-KEY.
     03  WK-TRDATE           PIC  9(08)     VALUE  ZERO.
     03  WK-TRTIME           PIC  9(06)     VALUE  ZERO.
*システム日付の編集
 01  WK-SYS-DATE.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*ステータス
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  SW3-STATUS        PIC  X(02).
     03  SW2-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MS1-STATUS        PIC  X(02).
     03  PL1-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "STB0030L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STB0030L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "STB0030L".
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
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HD00.
     03  FILLER         CHARACTER  TYPE YB-22.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "STB0030L".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  HD00-MODE       PIC  N(05).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(11)     VALUE
                             NC"＜　商品変換テーブル　".
         05  HD00-OUTSYUN    PIC  N(11).
     03  FILLER         CHARACTER  TYPE YA.
*********05  FILLER          PIC  X(10)     VALUE  SPACE.
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  HD00-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD00-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD00-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD00-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD00-MS         PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)
                             VALUE  NC"取込担当者：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRBUMON   PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD000-TRTANTO   PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRTANNM   PIC  N(10).
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)
                             VALUE  NC"取込日付　：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-YYYY       PIC  9(04).
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  "/".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)
                             VALUE  NC"取込時刻：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD01-HH         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-SS         PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD01-MS         PIC  9(02).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(04)     VALUE
             NC"_取引先".
         05  FILLER          PIC  X(02)     VALUE
               "M ".
         05  FILLER          PIC  N(03)     VALUE
             NC"_変換".
         05  FILLER          PIC  X(04)     VALUE
               "TBL ".
         05  FILLER          PIC  N(03)     VALUE
             NC"_名称".
         05  FILLER          PIC  X(02)     VALUE
               "M ".
         05  FILLER          PIC  N(03)     VALUE
             NC"_倉庫".
         05  FILLER          PIC  X(02)     VALUE
               "M ".
         05  FILLER          PIC  N(04)     VALUE
             NC"_整合性".
         05  FILLER          PIC  X(01)     VALUE
               " ".
         05  FILLER          PIC  N(05)     VALUE
             NC"_必須項目".
         05  FILLER          PIC  X(01)     VALUE
               " ".
         05  FILLER          PIC  N(02)     VALUE
             NC"_　".
         05  FILLER          PIC  N(02)     VALUE
             NC"_　".
         05  FILLER          PIC  N(02)     VALUE
             NC"_　".
         05  FILLER          PIC  N(03)     VALUE
             NC"_正常".
*
 01  HD02.
     03  FILLER              PIC  X(141)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(115)   VALUE  SPACE.
*        05  FILLER          PIC  N(10)
*                            VALUE NC"＜　エラー情報　　＞".
         05  FILLER          PIC  N(08)
                             VALUE NC"　＜－－エラー情".
         05  FILLER          PIC  N(04)
                             VALUE NC"報－－＞".
 01  HD04.
     03  FILLER         CHARACTER TYPE YB.
*        05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  N(05)  VALUE NC"取引先ＣＤ".
         05  FILLER        PIC  X(18)  VALUE SPACE.
         05  FILLER        PIC  N(07)  VALUE NC"量販店商品ＣＤ".
         05  FILLER        PIC  X(04)  VALUE SPACE.
         05  FILLER        PIC  N(06)  VALUE NC"自社商品ＣＤ".
         05  FILLER        PIC  X(12)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"出場".
         05  FILLER        PIC  X(03)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"原価単価".
         05  FILLER        PIC  X(04)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"売価単価".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"分類".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"棚番".
         05  FILLER        PIC  X(08)  VALUE SPACE.
         05  FILLER        PIC  N(04)  VALUE NC"仕入単価".
         05  FILLER        PIC  X(03)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"検品".
         05  FILLER        PIC  X(01)  VALUE "G".
         05  FILLER        PIC  X(03)  VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
**       05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"_".
*                            VALUE NC"__________"
*        05  FILLER        PIC  N(10)
*                            VALUE NC"_　_　_　_　_　"
*        05  FILLER        PIC  N(10)
*                            VALUE NC"_　_　_　_　_　".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TOKCD      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TOKNM      PIC  N(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-RSYOCD     PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-JSYOCD     PIC  X(21).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BASYO      PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-GTANKA     PIC  X(09).
         05  MS01-GTANKAR    REDEFINES  MS01-GTANKA.
             07 MS01-GTANKA9 PIC  Z,ZZZ,ZZ9.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BTANKA     PIC  X(09).
         05  MS01-BTANKAR    REDEFINES  MS01-BTANKA.
             07 MS01-BTANKA9 PIC  Z,ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-BUNRUI     PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANA       PIC  X(06).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-STANKA     PIC  X(09).
         05  MS01-STANKAR    REDEFINES  MS01-STANKA.
             07 MS01-STANKA9 PIC  Z,ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-KENPIN     PIC  X(05).
         05  MS01-KENPINR    REDEFINES  MS01-KENPIN.
             07 MS01-KENPIN9 PIC  ZZZZ9.
         05  FILLER          PIC  X(03)     VALUE  SPACE.
     03  FILLER              CHARACTER TYPE YB.
         05  MS01-ERR1       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR2       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR3       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR4       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR5       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR6       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR7       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR8       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR9       PIC  N(01).
**       05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR10      PIC  N(01).
*#2018/10/15 NAV ST
*検印欄１
 01  KEN01
     05  FILLER              PIC  X(117) VALUE SPACE.
     05  FILLER              PIC  X(19)  VALUE
         "+-----+-----+-----+".
*検印欄２
 01  KEN02
     05  FILLER              PIC  X(117) VALUE SPACE.
     05  FILLER              PIC  X(19)  VALUE
         "!     !     !     !".
*#2018/10/15 NAV ED
*
*対象データなし
 01  LST-DATA-X.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃".
 01  LST-DATA-Y.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　取込データはありません。　　　　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(141)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(141)    VALUE  ALL   "=".
*時刻編集
 01  SYS-TIME                PIC  9(08).
 01  WK-TIME      REDEFINES  SYS-TIME.
   03  WK-TIME-HM            PIC  9(06).
   03  WK-TIME-FIL           PIC  X(02).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-BUMON         PIC   X(04).
 01  PARA-IN-TANCD         PIC   X(02).
 01  PARA-IN-OUTSYU        PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-OUTSYU.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   STBXXXW3.
     MOVE      "STBXXXW3"   TO   AB-FILE.
     MOVE      SW3-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   STBXXXW2.
     MOVE      "STBXXXW2"   TO   AB-FILE.
     MOVE      SW2-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1  "   TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   PRTF.
     MOVE      "PRTF    "   TO   AB-FILE.
     MOVE      PRT-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     IF       PARA-IN-OUTSYU  =  "1"
              OPEN     INPUT  STBXXXW3  TANMS1
              OPEN     OUTPUT PRTF
     ELSE
              OPEN     INPUT  STBXXXW2  TANMS1
              OPEN     OUTPUT PRTF
     END-IF.
*
     DISPLAY  MSG-START UPON CONS.
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
     ACCEPT   SYS-TIME          FROM   TIME.
*
*商品変換テーブル取込ファイルスタート
     IF  PARA-IN-OUTSYU  = "1"
         PERFORM  STBXXXW3-START-SEC
     ELSE
         PERFORM  STBXXXW2-START-SEC
     END-IF.
     IF  END-FLG = "END"
         DISPLAY NC"＃＃　取込データ　なし　１＃＃" UPON CONS
         PERFORM  HEAD-WT-SEC
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
         MOVE    "END"         TO    END-FLG
         GO                    TO    INIT-EXIT
     END-IF.
*
*商品変換テーブル取込ファイル読込
     IF  PARA-IN-OUTSYU  = "1"
         PERFORM STBXXXW3-READ-SEC
     ELSE
         PERFORM STBXXXW2-READ-SEC
     END-IF.
     IF  END-FLG = "END"
         DISPLAY NC"＃＃　取込データ　なし　２＃＃"  UPON CONS
         PERFORM  HEAD-WT-SEC
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
         WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
         MOVE    "END"         TO    END-FLG
         GO                    TO    INIT-EXIT
     END-IF.
*
     MOVE         SPACE        TO    BRK-MODE.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
* 商品変換テーブル取込ファイル3 スタート
****************************************************************
 STBXXXW3-START-SEC          SECTION.
*
     MOVE    "STBXXXW3-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   SW3-REC.
     INITIALIZE                        SW3-REC.
*
     MOVE     " "                 TO   SW3-F11.
     MOVE     ZERO                TO   SW3-F13.
     MOVE     SPACE               TO   SW3-F14.
*
     START  STBXXXW3  KEY  IS  >=      SW3-F11  SW3-F13
                                       SW3-F14
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 STBXXXW3-START-EXIT.
     EXIT.
*
****************************************************************
*    商品変換テーブル取込ファイル3   読込
****************************************************************
 STBXXXW3-READ-SEC           SECTION.
*
     MOVE    "STBXXXW3-READ-SEC"   TO   S-NAME.
*
     READ     STBXXXW3  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   STBXXXW3-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   STBXXXW3-READ-CNT.
*
*モードブレイク判定
     IF       SW3-F11   NOT =  BK-F11
              MOVE     "BRK"      TO   BRK-MODE
              MOVE      SW3-F11   TO   BK-F11
     ELSE
              MOVE     "   "      TO   BRK-MODE
     END-IF.
*
 STBXXXW3-READ-EXIT.
     EXIT.
*
****************************************************************
* 商品変換テーブル取込ファイル2 スタート
****************************************************************
 STBXXXW2-START-SEC          SECTION.
*
     MOVE    "STBXXXW2-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   SW2-REC.
     INITIALIZE                        SW2-REC.
*
     MOVE     "1"                 TO   SW2-F060.
     MOVE     " "                 TO   SW2-F11.
     MOVE     ZERO                TO   SW2-F13.
     MOVE     SPACE               TO   SW2-F14.
*
     START  STBXXXW2  KEY  IS  >=      SW2-F060 SW2-F11
                                       SW2-F13  SW2-F14
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 STBXXXW2-START-EXIT.
     EXIT.
*
****************************************************************
*    商品変換テーブル取込ファイル2   読込
****************************************************************
 STBXXXW2-READ-SEC           SECTION.
*
     MOVE    "STBXXXW2-READ-SEC"   TO   S-NAME.
*
     READ     STBXXXW2  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   STBXXXW2-READ-EXIT
     END-READ.
*エラー区分チェック
*    DISPLAY "SW2-F060 = "SW2-F060 UPON CONS.
     IF       SW2-F060  NOT =  "1"
              MOVE     "END"      TO   END-FLG
*             DISPLAY "BBB" UPON CONS
              GO                  TO   STBXXXW2-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   STBXXXW2-READ-CNT.
*モードブレイク判定
     IF       SW2-F11   NOT =  BK-F11
              MOVE     "BRK"      TO   BRK-MODE
              MOVE      SW2-F11   TO   BK-F11
     ELSE
              MOVE     "   "      TO   BRK-MODE
     END-IF.
*
 STBXXXW2-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-010.
*印刷処理
     PERFORM  MEISAI-WT-SEC.
     MOVE     SPACE  TO   PRT-REC.
*
 MAIN-020.
     IF  PARA-IN-OUTSYU  = "1"
         PERFORM  STBXXXW3-READ-SEC
     ELSE
         PERFORM  STBXXXW2-READ-SEC
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    ヘッダ印字
*--------------------------------------------------------------*
 HEAD-WT-SEC            SECTION.
     MOVE    "HEAD-WT-SEC"        TO   S-NAME.
*    改頁判定
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
     END-IF.
*    行カウンター初期化
     MOVE     ZERO                TO   LINE-CNT.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    モード名セット
     IF  PARA-IN-OUTSYU  = "1"
         IF   SW3-F11    = "1"
              MOVE NC"【登録分】"      TO   HD00-MODE
         ELSE
              IF   SW3-F11    = "2"
                   MOVE NC"【更新分】" TO   HD00-MODE
              END-IF
         END-IF
     ELSE
         IF   SW2-F11    = "1"
              MOVE NC"【登録分】"      TO   HD00-MODE
         ELSE
              IF   SW2-F11    = "2"
                   MOVE NC"【更新分】" TO   HD00-MODE
         END-IF
     END-IF.
*    帳票名セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE NC"取込チェックリスト　＞"    TO   HD00-OUTSYUN
     ELSE
         MOVE NC"取込エラーリスト　＞"      TO   HD00-OUTSYUN
     END-IF.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD00-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD00-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD00-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD00-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD00-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD00-MS.
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TRBUMON.
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TRTANTO.
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = SPACE
               MOVE  TAN-F03      TO   HD000-TRTANNM
     ELSE
               MOVE  ALL NC"？"   TO   HD000-TRTANNM
     END-IF.
*    取込日付セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE SW3-F01(1:4)      TO   HD01-YYYY
         MOVE SW3-F01(5:2)      TO   HD01-MM
         MOVE SW3-F01(7:2)      TO   HD01-DD
     ELSE
         MOVE SW2-F01(1:4)      TO   HD01-YYYY
         MOVE SW2-F01(5:2)      TO   HD01-MM
         MOVE SW2-F01(7:2)      TO   HD01-DD
     END-IF.
*    時刻セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE SW3-F02(1:2)      TO   HD01-HH
         MOVE SW3-F02(3:2)      TO   HD01-SS
         MOVE SW3-F02(5:2)      TO   HD01-MS
     ELSE
         MOVE SW2-F02(1:2)      TO   HD01-HH
         MOVE SW2-F02(3:2)      TO   HD01-SS
         MOVE SW2-F02(5:2)      TO   HD01-MS
     END-IF.
*    ヘッダ印刷
*#2018/1015 NAV ST
     WRITE    PRT-REC       FROM  KEN01 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN01 AFTER  1.
*#2018/1015 NAV ED
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     15                  TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF     ( LINE-CNT  >   50  ) OR
            ( BRK-MODE = "BRK" )
              PERFORM HEAD-WT-SEC
     END-IF.
*   取引先コード
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  SW3-F13      TO   MS01-TOKCD
     ELSE
         MOVE  SW2-F13      TO   MS01-TOKCD
     END-IF.
*    取引先名
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  SW3-F51      TO   MS01-TOKNM
     ELSE
         MOVE  SW2-F51      TO   MS01-TOKNM
     END-IF.
*    量販店商品コード
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  SW3-F14      TO   MS01-RSYOCD
     ELSE
         MOVE  SW2-F14      TO   MS01-RSYOCD
     END-IF.
*    自社商品コード
     MOVE  "("                        TO   MS01-JSYOCD(1:1).
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F152         TO   MS01-JSYOCD(2:8)
         ELSE
               IF    SW3-F151  = "Y"
                     MOVE  SW3-F152   TO   MS01-JSYOCD(2:8)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(2:8)
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F152         TO   MS01-JSYOCD(2:8)
         ELSE
               IF    SW2-F151  = "Y"
                     MOVE  SW2-F152   TO   MS01-JSYOCD(2:8)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(2:8)
               END-IF
         END-IF
     END-IF.
*    品単1
     MOVE  "-"                        TO   MS01-JSYOCD(10:1).
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F162         TO   MS01-JSYOCD(11:5)
         ELSE
               IF    SW3-F161  = "Y"
                     MOVE  SW3-F162   TO   MS01-JSYOCD(11:5)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(11:5)
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F162         TO   MS01-JSYOCD(11:5)
         ELSE
               IF    SW2-F161  = "Y"
                     MOVE  SW2-F162   TO   MS01-JSYOCD(11:5)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(11:5)
               END-IF
         END-IF
     END-IF.
*    品単2
     MOVE  "-"                        TO   MS01-JSYOCD(16:1).
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F172         TO   MS01-JSYOCD(17:2)
         ELSE
               IF    SW3-F171  = "Y"
                     MOVE  SW3-F172   TO   MS01-JSYOCD(17:2)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(17:2)
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F172         TO   MS01-JSYOCD(17:2)
         ELSE
               IF    SW2-F171  = "Y"
                     MOVE  SW2-F172   TO   MS01-JSYOCD(17:2)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(17:2)
               END-IF
         END-IF
     END-IF.
*    品単3
     MOVE  "-"                        TO   MS01-JSYOCD(19:1).
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F182         TO   MS01-JSYOCD(20:1)
         ELSE
               IF    SW3-F181  = "Y"
                     MOVE  SW3-F182   TO   MS01-JSYOCD(20:1)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(20:1)
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F182         TO   MS01-JSYOCD(20:1)
         ELSE
               IF    SW2-F181  = "Y"
                     MOVE  SW2-F182   TO   MS01-JSYOCD(20:1)
               ELSE
                     MOVE  SPACE      TO   MS01-JSYOCD(20:1)
               END-IF
         END-IF
     END-IF.
     MOVE  ")"                        TO   MS01-JSYOCD(21:1).
*   出荷場所
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F192         TO   MS01-BASYO
         ELSE
               IF    SW3-F191  = "Y"
                     MOVE  SW3-F192   TO   MS01-BASYO
               ELSE
                     MOVE  SPACE      TO   MS01-BASYO
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F192         TO   MS01-BASYO
         ELSE
               IF    SW2-F191  = "Y"
                     MOVE  SW2-F192   TO   MS01-BASYO
               ELSE
                     MOVE  SPACE      TO   MS01-BASYO
               END-IF
         END-IF
     END-IF.
*   原価単価
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F202         TO   MS01-GTANKA9
         ELSE
               IF    SW3-F201  = "Y"
                     MOVE  SW3-F202   TO   MS01-GTANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-GTANKA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F202         TO   MS01-GTANKA9
         ELSE
               IF    SW2-F201  = "Y"
                     MOVE  SW2-F202   TO   MS01-GTANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-GTANKA
               END-IF
         END-IF
     END-IF.
*   売価単価
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F212         TO   MS01-BTANKA9
         ELSE
               IF    SW3-F211  = "Y"
                     MOVE  SW3-F212   TO   MS01-BTANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-BTANKA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F212         TO   MS01-BTANKA9
         ELSE
               IF    SW2-F211  = "Y"
                     MOVE  SW2-F212   TO   MS01-BTANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-BTANKA
               END-IF
         END-IF
     END-IF.
*   分類コード
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F222         TO   MS01-BUNRUI
         ELSE
               IF    SW3-F221  = "Y"
                     MOVE  SW3-F222   TO   MS01-BUNRUI
               ELSE
                     MOVE  SPACE      TO   MS01-BUNRUI
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F222         TO   MS01-BUNRUI
         ELSE
               IF    SW2-F221  = "Y"
                     MOVE  SW2-F222   TO   MS01-BUNRUI
               ELSE
                     MOVE  SPACE      TO   MS01-BUNRUI
               END-IF
         END-IF
     END-IF.
*   棚番
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F232         TO   MS01-TANA
         ELSE
               IF    SW3-F231  = "Y"
                     MOVE  SW3-F232   TO   MS01-TANA
               ELSE
                     MOVE  SPACE      TO   MS01-TANA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F232         TO   MS01-TANA
         ELSE
               IF    SW2-F231  = "Y"
                     MOVE  SW2-F232   TO   MS01-TANA
               ELSE
                     MOVE  SPACE      TO   MS01-TANA
               END-IF
         END-IF
     END-IF.
*   仕入単価
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F242         TO   MS01-STANKA9
         ELSE
               IF    SW3-F241  = "Y"
                     MOVE  SW3-F242   TO   MS01-STANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-STANKA
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F242         TO   MS01-STANKA9
         ELSE
               IF    SW2-F241  = "Y"
                     MOVE  SW2-F242   TO   MS01-STANKA9
               ELSE
                     MOVE  SPACE      TO   MS01-STANKA
               END-IF
         END-IF
     END-IF.
*   検品Ｇ
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F11   = "1"
               MOVE  SW3-F252         TO   MS01-KENPIN9
         ELSE
               IF    SW3-F251  = "Y"
                     MOVE  SW3-F252   TO   MS01-KENPIN9
               ELSE
                     MOVE  SPACE      TO   MS01-KENPIN
               END-IF
         END-IF
     ELSE
         IF    SW2-F11   = "1"
               MOVE  SW2-F252         TO   MS01-KENPIN9
         ELSE
               IF    SW2-F251  = "Y"
                     MOVE  SW2-F252   TO   MS01-KENPIN9
               ELSE
                     MOVE  SPACE      TO   MS01-KENPIN
               END-IF
         END-IF
     END-IF.
*   エラー区分１
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F061 = " "
               MOVE SPACE        TO   MS01-ERR1
         ELSE
               MOVE NC"×"       TO   MS01-ERR1
         END-IF
     ELSE
         IF    SW2-F061 = " "
               MOVE SPACE        TO   MS01-ERR1
         ELSE
               MOVE NC"×"       TO   MS01-ERR1
         END-IF
     END-IF.
*    エラー区分２
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F062 = " "
               MOVE SPACE        TO   MS01-ERR2
         ELSE
               MOVE NC"×"       TO   MS01-ERR2
         END-IF
     ELSE
         IF    SW2-F062 = " "
               MOVE SPACE        TO   MS01-ERR2
         ELSE
               MOVE NC"×"       TO   MS01-ERR2
         END-IF
     END-IF.
*    エラー区分３
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F063 = " "
               MOVE SPACE        TO   MS01-ERR3
         ELSE
               MOVE NC"×"       TO   MS01-ERR3
         END-IF
     ELSE
         IF    SW2-F063 = " "
               MOVE SPACE        TO   MS01-ERR3
         ELSE
               MOVE NC"×"       TO   MS01-ERR3
         END-IF
     END-IF.
*    エラー区分４
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F064 = " "
               MOVE SPACE        TO   MS01-ERR4
         ELSE
               MOVE NC"×"       TO   MS01-ERR4
         END-IF
     ELSE
         IF    SW2-F064 = " "
               MOVE SPACE        TO   MS01-ERR4
         ELSE
               MOVE NC"×"       TO   MS01-ERR4
         END-IF
     END-IF.
*    エラー区分５
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F065 = " "
               MOVE SPACE        TO   MS01-ERR5
         ELSE
               MOVE NC"×"       TO   MS01-ERR5
         END-IF
     ELSE
         IF    SW2-F065 = " "
               MOVE SPACE        TO   MS01-ERR5
         ELSE
               MOVE NC"×"       TO   MS01-ERR5
         END-IF
     END-IF.
*    エラー区分６
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F066 = " "
               MOVE SPACE        TO   MS01-ERR6
         ELSE
               MOVE NC"×"       TO   MS01-ERR6
         END-IF
     ELSE
         IF    SW2-F066 = " "
               MOVE SPACE        TO   MS01-ERR6
         ELSE
               MOVE NC"×"       TO   MS01-ERR6
         END-IF
     END-IF.
*    エラー区分７
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F067 = " "
               MOVE SPACE        TO   MS01-ERR7
         ELSE
               MOVE NC"×"       TO   MS01-ERR7
         END-IF
     ELSE
         IF    SW2-F067 = " "
               MOVE SPACE        TO   MS01-ERR7
         ELSE
               MOVE NC"×"       TO   MS01-ERR7
         END-IF
     END-IF.
*    エラー区分８
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F068 = " "
               MOVE SPACE        TO   MS01-ERR8
         ELSE
               MOVE NC"×"       TO   MS01-ERR8
         END-IF
     ELSE
         IF    SW2-F068 = " "
               MOVE SPACE        TO   MS01-ERR8
         ELSE
               MOVE NC"×"       TO   MS01-ERR8
         END-IF
     END-IF.
*    エラー区分９
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F069 = " "
               MOVE SPACE        TO   MS01-ERR9
         ELSE
               MOVE NC"×"       TO   MS01-ERR9
         END-IF
     ELSE
         IF    SW2-F069 = " "
               MOVE SPACE        TO   MS01-ERR9
         ELSE
               MOVE NC"×"       TO   MS01-ERR9
         END-IF
     END-IF.
*    エラー区分１０("1"＝エラー無し)
     IF  PARA-IN-OUTSYU  = "1"
         IF    SW3-F06A = " "
               MOVE SPACE        TO   MS01-ERR10
         ELSE
               MOVE NC"○"       TO   MS01-ERR10
         END-IF
     ELSE
         IF    SW2-F06A = " "
               MOVE SPACE        TO   MS01-ERR10
         ELSE
               MOVE NC"○"       TO   MS01-ERR10
         END-IF
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*商品変換テーブル
     IF  PARA-IN-OUTSYU  = "1"
         DISPLAY "STBXXXW3 PRINT-CNT = " STBXXXW3-READ-CNT
                                                       UPON CONS
     ELSE
         DISPLAY "STBXXXW2 PRINT-CNT = " STBXXXW2-READ-CNT
                                                       UPON CONS
     END-IF.
*
     IF       PARA-IN-OUTSYU  =  "1"
              CLOSE  STBXXXW3  TANMS1  PRTF
     ELSE
              CLOSE  STBXXXW2  TANMS1  PRTF
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC           SECTION.
     MOVE "TANMS1-READ-SEC"       TO   S-NAME.
*
     READ       TANMS1
         INVALID       MOVE "INV" TO   TANMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
