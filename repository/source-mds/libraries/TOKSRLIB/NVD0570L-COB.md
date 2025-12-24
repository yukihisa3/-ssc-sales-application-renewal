# NVD0570L

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/NVD0570L.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　在庫　　　　　　　　　　　　　　  *
*    モジュール名　　　　：（ＥＸＣＥＬ取込）　　　　　　　　　*
*    　　　　　　　　　　　　倉庫間移動指示　取込チェックリスト*
*    作成日／作成者　　　：　2022/09/02 INOUE                  *
*    処理概要　　　　　　：　取込チェック結果をリスト出力する。*
*                            （全件・エラー分をＰＡＲＡ制御）　*
*    流用元　　　　　　　：　NVM0130L.TOKSRLIB                 *
*    更新日／更新者　　　：　                                  *
*    更新内容　　　　　　：　                                  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NVD0570L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/09/02.
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
*倉庫間移動指示  取込ファイル1
     SELECT   IDOXXXW1  ASSIGN    TO        DA-01-VI-IDOXXXW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IW1-F05  IW1-F07
                                            IW1-F08  IW1-F09
                                            IW1-F10
                        FILE  STATUS   IS   IW1-STATUS.
*倉庫間移動指示  取込ファイル2
     SELECT   IDOXXXW2  ASSIGN    TO        DA-01-VI-IDOXXXW2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IW2-F040
                                            IW2-F05  IW2-F07
                                            IW2-F08  IW2-F09
                                            IW2-F10
                        FILE  STATUS   IS   IW2-STATUS.
*担当者マスタ
     SELECT     TANMS1       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*サブ商品名称マスタ
     SELECT      SUBMEIL7    ASSIGN    TO       DA-01-VI-SUBMEIL7
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      SBM-D01
                             FILE      STATUS   SBM-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*---------------------------------------------------------------
 DATA                   DIVISION.
 FILE                   SECTION.
*倉庫間移動指示  取込ファイル1
 FD  IDOXXXW1           LABEL RECORD   IS   STANDARD.
     COPY     IDOXXXW1  OF        XFDLIB
              JOINING   IW1       PREFIX.
*倉庫間移動指示  取込ファイル2
 FD  IDOXXXW2           LABEL RECORD   IS   STANDARD.
     COPY     IDOXXXW2  OF        XFDLIB
              JOINING   IW2       PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*サブ商品名称マスタ
 FD  SUBMEIL7           LABEL RECORD   IS   STANDARD.
     COPY        SUBMEIL7    OF        XFDLIB
     JOINING     SBM         AS        PREFIX.
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
 01  IDOXXXW1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  IDOXXXW2-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  SBM-INV-FLG             PIC  X(03)     VALUE  SPACE.
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
     03  IW1-STATUS        PIC  X(02).
     03  IW2-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MS1-STATUS        PIC  X(02).
     03  PL1-STATUS        PIC  X(02).
     03  SBM-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NVD0570L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVD0570L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NVD0570L".
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
         05  FILLER          PIC  X(08)     VALUE  "NVD0570L".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  HD00-MODE       PIC  N(05).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(11)     VALUE
                             NC"＜　倉庫間移動指示　　".
         05  HD00-OUTSYUN    PIC  N(11).
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(10)     VALUE  SPACE.
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
         05  FILLER          PIC  X(18)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(06)     VALUE
             NC"１移動元倉庫".
         05  FILLER          PIC  X(01)     VALUE
               " ".
         05  FILLER          PIC  N(06)     VALUE
             NC"２移動先倉庫".
         05  FILLER          PIC  X(01)     VALUE
               " ".
         05  FILLER          PIC  N(04)     VALUE
             NC"３移動日".
         05  FILLER          PIC  X(01)     VALUE
               " ".
         05  FILLER          PIC  N(06)     VALUE
             NC"４移動日範囲".
         05  FILLER          PIC  X(01)     VALUE
               " ".
         05  FILLER          PIC  N(03)     VALUE
             NC"５名称".
         05  FILLER          PIC  X(02)     VALUE
               "M ".
         05  FILLER          PIC  N(03)     VALUE
             NC"６数量".
         05  FILLER          PIC  X(02)     VALUE
               "  ".
         05  FILLER          PIC  N(02)     VALUE
             NC"７　".
         05  FILLER          PIC  N(02)     VALUE
             NC"８　".
         05  FILLER          PIC  N(02)     VALUE
             NC"９　".
         05  FILLER          PIC  N(03)     VALUE
             NC"０正常".
*
 01  HD02.
     03  FILLER              PIC  X(141)    VALUE  ALL "=".
 01  HD021.
     03  FILLER              PIC  X(141)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(115)   VALUE  SPACE.
         05  FILLER          PIC  N(09)
                             VALUE NC"　＜－－－エラー情".
         05  FILLER          PIC  N(07)
                             VALUE NC"報　－－－＞　".
 01  HD04.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER        PIC  N(04)  VALUE NC"倉庫ＣＤ".
         05  FILLER        PIC  X(04)  VALUE SPACE.
         05  FILLER        PIC  N(03)  VALUE NC"移動日".
         05  FILLER        PIC  X(07)  VALUE SPACE.
         05  FILLER        PIC  N(05)  VALUE NC"ＪＡＮＣＤ".
         05  FILLER        PIC  X(08)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"_番".
         05  FILLER        PIC  X(16)  VALUE SPACE.
         05  FILLER        PIC  N(05)  VALUE NC"移動指示数".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  N(02)  VALUE NC"出区".
         05  FILLER        PIC  X(06)  VALUE SPACE.
         05  FILLER        PIC  N(03)  VALUE NC"出庫数".
         05  FILLER        PIC  X(03)  VALUE SPACE.
         05  FILLER        PIC  X(05)  VALUE   "ｽﾄｯｸ1".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  X(05)  VALUE   "ｽﾄｯｸ2".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  X(05)  VALUE   "ｽﾄｯｸ3".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  X(05)  VALUE   "ｽﾄｯｸ4".
         05  FILLER        PIC  X(02)  VALUE SPACE.
         05  FILLER        PIC  X(05)  VALUE   "ｽﾄｯｸ5".
         05  FILLER        PIC  X(02)  VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER        PIC  N(01)  VALUE NC"１".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"２".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"３".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"４".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"５".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"６".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"７".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"８".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"９".
*        05  FILLER        PIC  X(01)  VALUE SPACE.
         05  FILLER        PIC  N(01)  VALUE NC"０".
*
 01  HD05.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER     PIC  N(01) VALUE NC"元".
         05  FILLER     PIC  X(01) VALUE SPACE.
         05  FILLER     PIC  N(01) VALUE NC"→".
         05  FILLER     PIC  X(01) VALUE SPACE.
         05  FILLER     PIC  N(01) VALUE NC"先".
         05  FILLER     PIC  X(31) VALUE SPACE.
         05  FILLER     PIC  N(01) VALUE NC"出".
         05  FILLER     PIC  X(05) VALUE SPACE.
         05  FILLER     PIC  N(01) VALUE NC"→".
         05  FILLER     PIC  X(01) VALUE SPACE.
         05  FILLER     PIC  N(01) VALUE NC"入".
         05  FILLER     PIC  X(35) VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER     PIC  N(02) VALUE NC"数量".
         05  FILLER     PIC  X(01) VALUE   "1".
         05  FILLER     PIC  X(03) VALUE SPACE.
         05  FILLER     PIC  N(02) VALUE NC"数量".
         05  FILLER     PIC  X(01) VALUE   "2".
         05  FILLER     PIC  X(03) VALUE SPACE.
         05  FILLER     PIC  N(02) VALUE NC"数量".
         05  FILLER     PIC  X(01) VALUE   "3".
         05  FILLER     PIC  X(03) VALUE SPACE.
         05  FILLER     PIC  N(02) VALUE NC"数量".
         05  FILLER     PIC  X(01) VALUE   "4".
         05  FILLER     PIC  X(03) VALUE SPACE.
         05  FILLER     PIC  N(02) VALUE NC"数量".
         05  FILLER     PIC  X(01) VALUE   "5".
*
 01  MS01.
     03  FILLER              CHARACTER TYPE YB.
         05  MS01-MSOKCD     PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"→".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SSOKCD     PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-IDODATE    PIC  X(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-JANCD      PIC  X(13).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-MTANA      PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"→".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STANA      PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-IDOSIJI    PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SKUBUN     PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SYUKKO     PIC  ZZZ,ZZZ,ZZ9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-STOCK1     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STOCK2     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STOCK3     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STOCK4     PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-STOCK5     PIC  X(06).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
     03  FILLER              CHARACTER TYPE YB.
         05  MS01-ERR1       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR2       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR3       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR4       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR5       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR6       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR7       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR8       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR9       PIC  N(01).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-ERR10      PIC  N(01).
 01  MS02.
     03  FILLER              CHARACTER TYPE YB.
         05  FILLER          PIC  X(22)     VALUE  SPACE.
         05  MS02-SHONAME.
             07  MS02-SHONAME1   PIC  N(15).
             07  MS02-SHONAME2   PIC  N(15).
         05  FILLER          PIC  X(14)     VALUE  SPACE.
         05  MS02-STOCK1     PIC  ZZZZZZ.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-STOCK2     PIC  ZZZZZZ.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-STOCK3     PIC  ZZZZZZ.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-STOCK4     PIC  ZZZZZZ.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-STOCK5     PIC  ZZZZZZ.
*
*検印欄１
 01  KEN01.
     05  FILLER              PIC  X(117) VALUE SPACE.
     05  FILLER              PIC  X(19)  VALUE
         "+-----+-----+-----+".
*検印欄２
 01  KEN02.
     05  FILLER              PIC  X(117) VALUE SPACE.
     05  FILLER              PIC  X(19)  VALUE
         "!     !     !     !".
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
                        PROCEDURE   IDOXXXW1.
     MOVE      "IDOXXXW1"   TO   AB-FILE.
     MOVE      IW1-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   IDOXXXW2.
     MOVE      "IDOXXXW2"   TO   AB-FILE.
     MOVE      IW2-STATUS   TO   AB-STS.
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
                        PROCEDURE   SUBMEIL7.
     MOVE      "SUBMEIL7"   TO   AB-FILE.
     MOVE      SBM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
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
              OPEN     INPUT  IDOXXXW1  TANMS1 SUBMEIL7
              OPEN     OUTPUT PRTF
     ELSE
              OPEN     INPUT  IDOXXXW2  TANMS1 SUBMEIL7
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
         PERFORM  IDOXXXW1-START-SEC
     ELSE
         PERFORM  IDOXXXW2-START-SEC
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
         PERFORM IDOXXXW1-READ-SEC
     ELSE
         PERFORM IDOXXXW2-READ-SEC
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
* 商品変換テーブル取込ファイル1 スタート
****************************************************************
 IDOXXXW1-START-SEC          SECTION.
*
     MOVE    "IDOXXXW1-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   IW1-REC.
     INITIALIZE                        IW1-REC.
*
     MOVE     SPACE               TO   IW1-F05
                                       IW1-F07
                                       IW1-F08
                                       IW1-F09
                                       IW1-F10.
*
     START  IDOXXXW1  KEY  IS  >=      IW1-F05  IW1-F07  IW1-F08
                                       IW1-F09  IW1-F10
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 IDOXXXW1-START-EXIT.
     EXIT.
*
****************************************************************
*    商品変換テーブル取込ファイル1   読込
****************************************************************
 IDOXXXW1-READ-SEC           SECTION.
*
     MOVE    "IDOXXXW1-READ-SEC"   TO   S-NAME.
*
     READ     IDOXXXW1  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   IDOXXXW1-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   IDOXXXW1-READ-CNT.
*
*モードブレイク判定
*    IF       IW1-F11   NOT =  BK-F11
*             MOVE     "BRK"      TO   BRK-MODE
*             MOVE      IW1-F11   TO   BK-F11
*    ELSE
*             MOVE     "   "      TO   BRK-MODE
*    END-IF.
*
 IDOXXXW1-READ-EXIT.
     EXIT.
*
****************************************************************
* 商品変換テーブル取込ファイル2 スタート
****************************************************************
 IDOXXXW2-START-SEC          SECTION.
*
     MOVE    "IDOXXXW2-START-SEC" TO   S-NAME.
*
     MOVE     SPACE               TO   IW2-REC.
     INITIALIZE                        IW2-REC.
*
     MOVE     "1"                 TO   IW2-F040
     MOVE     SPACE               TO   IW2-F05
                                       IW2-F07
                                       IW2-F08
                                       IW2-F09
                                       IW2-F10.
*
     START  IDOXXXW2  KEY  IS  >=      IW2-F040 IW2-F05 IW2-F07
                                       IW2-F08  IW2-F09 IW2-F10
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 IDOXXXW2-START-EXIT.
     EXIT.
*
****************************************************************
*    商品変換テーブル取込ファイル2   読込
****************************************************************
 IDOXXXW2-READ-SEC           SECTION.
*
     MOVE    "IDOXXXW2-READ-SEC"   TO   S-NAME.
*
     READ     IDOXXXW2  AT  END
              MOVE     "END"      TO   END-FLG
*             DISPLAY "AAA" UPON CONS
              GO                  TO   IDOXXXW2-READ-EXIT
     END-READ.
*エラー区分チェック
*    DISPLAY "IW2-F040 = "IW2-F040 UPON CONS.
     IF       IW2-F040  NOT =  "1"
              MOVE     "END"      TO   END-FLG
*             DISPLAY "BBB" UPON CONS
              GO                  TO   IDOXXXW2-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   IDOXXXW2-READ-CNT.
*モードブレイク判定
*    IF       IW2-F11   NOT =  BK-F11
*             MOVE     "BRK"      TO   BRK-MODE
*             MOVE      IW2-F11   TO   BK-F11
*    ELSE
*             MOVE     "   "      TO   BRK-MODE
*    END-IF.
*
 IDOXXXW2-READ-EXIT.
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
         PERFORM  IDOXXXW1-READ-SEC
     ELSE
         PERFORM  IDOXXXW2-READ-SEC
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
         MOVE IW1-F01(1:4)      TO   HD01-YYYY
         MOVE IW1-F01(5:2)      TO   HD01-MM
         MOVE IW1-F01(7:2)      TO   HD01-DD
     ELSE
         MOVE IW2-F01(1:4)      TO   HD01-YYYY
         MOVE IW2-F01(5:2)      TO   HD01-MM
         MOVE IW2-F01(7:2)      TO   HD01-DD
     END-IF.
*    時刻セット
     IF  PARA-IN-OUTSYU  = "1"
         MOVE IW1-F02(1:2)      TO   HD01-HH
         MOVE IW1-F02(3:2)      TO   HD01-SS
         MOVE IW1-F02(5:2)      TO   HD01-MS
     ELSE
         MOVE IW2-F02(1:2)      TO   HD01-HH
         MOVE IW2-F02(3:2)      TO   HD01-SS
         MOVE IW2-F02(5:2)      TO   HD01-MS
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  KEN01 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN02 AFTER  1.
     WRITE    PRT-REC       FROM  KEN01 AFTER  1.
     WRITE    PRT-REC       FROM  HD00  AFTER  2.
     WRITE    PRT-REC       FROM  HD000 AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     16                  TO    LINE-CNT.
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
*    移動元倉庫ＣＤ
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F05      TO   MS01-MSOKCD
     ELSE
         MOVE  IW2-F05      TO   MS01-MSOKCD
     END-IF.
*    移動先倉庫ＣＤ
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F07      TO   MS01-SSOKCD
     ELSE
         MOVE  IW2-F07      TO   MS01-SSOKCD
     END-IF.
*    移動日
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F06(1:4) TO   MS01-IDODATE(1:4)
         MOVE  "/"          TO   MS01-IDODATE(5:1)
         MOVE  IW1-F06(5:2) TO   MS01-IDODATE(6:2)
         MOVE  "/"          TO   MS01-IDODATE(8:1)
         MOVE  IW1-F06(7:2) TO   MS01-IDODATE(9:2)
     ELSE
         MOVE  IW2-F06(1:4) TO   MS01-IDODATE(1:4)
         MOVE  "/"          TO   MS01-IDODATE(5:1)
         MOVE  IW2-F06(5:2) TO   MS01-IDODATE(6:2)
         MOVE  "/"          TO   MS01-IDODATE(8:1)
         MOVE  IW2-F06(7:2) TO   MS01-IDODATE(9:2)
     END-IF.
*    ＪＡＮＣＤ
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F08      TO   MS01-JANCD
     ELSE
         MOVE  IW2-F08      TO   MS01-JANCD
     END-IF.
*    出庫_番
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F09      TO   MS01-MTANA
     ELSE
         MOVE  IW2-F09      TO   MS01-MTANA
     END-IF.
*    入庫_番
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F10      TO   MS01-STANA
     ELSE
         MOVE  IW2-F10      TO   MS01-STANA
     END-IF.
*    移動指示数
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F11      TO   MS01-IDOSIJI
     ELSE
         MOVE  IW2-F11      TO   MS01-IDOSIJI
     END-IF.
*    出庫作成区分
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F12      TO   MS01-SKUBUN
     ELSE
         MOVE  IW2-F12      TO   MS01-SKUBUN
     END-IF.
*    出庫数
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F13      TO   MS01-SYUKKO
     ELSE
         MOVE  IW2-F13      TO   MS01-SYUKKO
     END-IF.
*    ストック_1
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F14      TO   MS01-STOCK1
     ELSE
         MOVE  IW2-F14      TO   MS01-STOCK1
     END-IF.
*    ストック_2
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F16      TO   MS01-STOCK2
     ELSE
         MOVE  IW2-F16      TO   MS01-STOCK2
     END-IF.
*    ストック_3
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F18      TO   MS01-STOCK3
     ELSE
         MOVE  IW2-F18      TO   MS01-STOCK3
     END-IF.
*    ストック_4
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F20      TO   MS01-STOCK4
     ELSE
         MOVE  IW2-F20      TO   MS01-STOCK4
     END-IF.
*    ストック_5
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F22      TO   MS01-STOCK5
     ELSE
         MOVE  IW2-F22      TO   MS01-STOCK5
     END-IF.
*
*    商品名
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F08      TO   SBM-D01
     ELSE
         MOVE  IW2-F08      TO   SBM-D01
     END-IF.
     PERFORM  SBM-READ-SEC
     IF       SBM-INV-FLG     =   "INV"
              MOVE   ALL NC"？"  TO   MS02-SHONAME
     ELSE
              MOVE   SBM-F021    TO   MS02-SHONAME1
              MOVE   SBM-F022    TO   MS02-SHONAME2
     END-IF.
*    数量内訳1
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F15      TO   MS02-STOCK1
     ELSE
         MOVE  IW2-F15      TO   MS02-STOCK1
     END-IF.
*    数量内訳2
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F17      TO   MS02-STOCK2
     ELSE
         MOVE  IW2-F17      TO   MS02-STOCK2
     END-IF.
*    数量内訳3
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F19      TO   MS02-STOCK3
     ELSE
         MOVE  IW2-F19      TO   MS02-STOCK3
     END-IF.
*    数量内訳4
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F21      TO   MS02-STOCK4
     ELSE
         MOVE  IW2-F21      TO   MS02-STOCK4
     END-IF.
*    数量内訳5
     IF  PARA-IN-OUTSYU  = "1"
         MOVE  IW1-F23      TO   MS02-STOCK5
     ELSE
         MOVE  IW2-F23      TO   MS02-STOCK5
     END-IF.
*
*   エラー区分１
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F041 = " "
               MOVE SPACE        TO   MS01-ERR1
         ELSE
               MOVE NC"×"       TO   MS01-ERR1
         END-IF
     ELSE
         IF    IW2-F041 = " "
               MOVE SPACE        TO   MS01-ERR1
         ELSE
               MOVE NC"×"       TO   MS01-ERR1
         END-IF
     END-IF.
*    エラー区分２
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F042 = " "
               MOVE SPACE        TO   MS01-ERR2
         ELSE
               MOVE NC"×"       TO   MS01-ERR2
         END-IF
     ELSE
         IF    IW2-F042 = " "
               MOVE SPACE        TO   MS01-ERR2
         ELSE
               MOVE NC"×"       TO   MS01-ERR2
         END-IF
     END-IF.
*    エラー区分３
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F043 = " "
               MOVE SPACE        TO   MS01-ERR3
         ELSE
               MOVE NC"×"       TO   MS01-ERR3
         END-IF
     ELSE
         IF    IW2-F043 = " "
               MOVE SPACE        TO   MS01-ERR3
         ELSE
               MOVE NC"×"       TO   MS01-ERR3
         END-IF
     END-IF.
*    エラー区分４
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F044 = " "
               MOVE SPACE        TO   MS01-ERR4
         ELSE
               MOVE NC"×"       TO   MS01-ERR4
         END-IF
     ELSE
         IF    IW2-F044 = " "
               MOVE SPACE        TO   MS01-ERR4
         ELSE
               MOVE NC"×"       TO   MS01-ERR4
         END-IF
     END-IF.
*    エラー区分５
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F045 = " "
               MOVE SPACE        TO   MS01-ERR5
         ELSE
               MOVE NC"×"       TO   MS01-ERR5
         END-IF
     ELSE
         IF    IW2-F045 = " "
               MOVE SPACE        TO   MS01-ERR5
         ELSE
               MOVE NC"×"       TO   MS01-ERR5
         END-IF
     END-IF.
*    エラー区分６
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F046 = " "
               MOVE SPACE        TO   MS01-ERR6
         ELSE
               MOVE NC"×"       TO   MS01-ERR6
         END-IF
     ELSE
         IF    IW2-F046 = " "
               MOVE SPACE        TO   MS01-ERR6
         ELSE
               MOVE NC"×"       TO   MS01-ERR6
         END-IF
     END-IF.
*    エラー区分７
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F047 = " "
               MOVE SPACE        TO   MS01-ERR7
         ELSE
               MOVE NC"×"       TO   MS01-ERR7
         END-IF
     ELSE
         IF    IW2-F047 = " "
               MOVE SPACE        TO   MS01-ERR7
         ELSE
               MOVE NC"×"       TO   MS01-ERR7
         END-IF
     END-IF.
*    エラー区分８
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F048 = " "
               MOVE SPACE        TO   MS01-ERR8
         ELSE
               MOVE NC"×"       TO   MS01-ERR8
         END-IF
     ELSE
         IF    IW2-F048 = " "
               MOVE SPACE        TO   MS01-ERR8
         ELSE
               MOVE NC"×"       TO   MS01-ERR8
         END-IF
     END-IF.
*    エラー区分９
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F049 = " "
               MOVE SPACE        TO   MS01-ERR9
         ELSE
               MOVE NC"×"       TO   MS01-ERR9
         END-IF
     ELSE
         IF    IW2-F049 = " "
               MOVE SPACE        TO   MS01-ERR9
         ELSE
               MOVE NC"×"       TO   MS01-ERR9
         END-IF
     END-IF.
*    エラー区分１０("1"＝エラー無し)
     IF  PARA-IN-OUTSYU  = "1"
         IF    IW1-F04A = " "
               MOVE SPACE        TO   MS01-ERR10
         ELSE
               MOVE NC"○"       TO   MS01-ERR10
         END-IF
     ELSE
         IF    IW2-F04A = " "
               MOVE SPACE        TO   MS01-ERR10
         ELSE
               MOVE NC"○"       TO   MS01-ERR10
         END-IF
     END-IF.
*
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  HD021 AFTER  1.
*行カウント
     ADD      3                   TO    LINE-CNT.
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
     IF  PARA-IN-OUTSYU  = "1"
         DISPLAY "IDOXXXW1 PRINT-CNT = " IDOXXXW1-READ-CNT
                                                       UPON CONS
     ELSE
         DISPLAY "IDOXXXW2 PRINT-CNT = " IDOXXXW2-READ-CNT
                                                       UPON CONS
     END-IF.
*
     IF       PARA-IN-OUTSYU  =  "1"
              CLOSE  IDOXXXW1  TANMS1  SUBMEIL7  PRTF
     ELSE
              CLOSE  IDOXXXW2  TANMS1  SUBMEIL7  PRTF
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
****************************************************************
*    サブ商品名称マスタ検索
****************************************************************
 SBM-READ-SEC               SECTION.
     MOVE    "SBM-READ-SEC" TO   S-NAME.
*
     READ     SUBMEIL7
       INVALID
              MOVE "INV"     TO   SBM-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   SBM-INV-FLG
     END-READ.
 SBM-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
