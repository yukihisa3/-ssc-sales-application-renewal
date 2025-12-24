****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　伝票ＥＸＣＥＬ取込エラーリスト    *
*    作成日／作成者　　　：　2016/09/20 INOUE                  *
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SDE0050L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/20.
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
*伝票EXCEL取込ファイル
     SELECT   DEXXXXL1  ASSIGN    TO        DA-01-VI-DEXXXXL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       EXL-F301
                                            EXL-F201
                        FILE  STATUS   IS   EXL-STATUS.
*担当者マスタ
     SELECT     TANMS1       ASSIGN    TO       DA-01-VI-TANMS1
                             ORGANIZATION       INDEXED
                             ACCESS    MODE     RANDOM
                             RECORD    KEY      TAN-F01  TAN-F02
                             FILE      STATUS   TAN-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    伝票EXCEL取込ファイル
 FD  DEXXXXL1            LABEL RECORD   IS   STANDARD.
     COPY     DEXXXXL1   OF        XFDLIB
              JOINING   EXL       PREFIX.
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
 01  DEXXXXL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  ZERO.
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
     03  EXL-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SDE0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0050L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0050L".
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
 01  HD0.
     03  FILLER         CHARACTER  TYPE YB-22.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SDE0050L".
         05  FILLER          PIC  X(27)     VALUE  SPACE.
         05  FILLER          PIC  N(17)     VALUE
         NC"＜伝票ＥＸＣＥＬ取込エラーリスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(19)     VALUE  SPACE.
         05  HD0-YYYY        PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD0-MM          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD0-DD          PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD0-HH          PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD0-SS          PIC  9(02).
         05  FILLER          PIC  X(01)     VALUE  ":".
         05  HD0-MS          PIC  9(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  HD0-PCNT        PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
 01  HD00.
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(44)     VALUE  SPACE.
         05  HD00-EXCEL      PIC  N(18).
 01  HD000.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(07)
                             VALUE  NC"取込担当者　：".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRBUMON   PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  "-".
         05  HD000-TRTANTO   PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD000-TRTANNM   PIC  N(10).
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(05)     VALUE
             NC"_必須項目".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_整合性".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_取引先Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_店舗Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_倉庫Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_変換ＴＢＬ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_名称Ｍ".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"_６行超".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_伝区　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_制区　".
         05  FILLER          PIC  N(04)     VALUE
             NC"_発注日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_納品日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_原単価".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_売単価".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
*
 01  HD02.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"連番".
         05  FILLER          PIC  X(07)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"伝票番号".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"行".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(02)     VALUE  NC"伝区".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(02)     VALUE  NC"店舗".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(02)     VALUE  NC"出場".
         05  FILLER          PIC  X(01)     VALUE  "･".
         05  FILLER          PIC  N(02)     VALUE  NC"伝場".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(03)     VALUE  NC"発注日".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"分類".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"相手商品".
         05  FILLER          PIC  X(11)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"数量".
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"原価単価".
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"売価単価".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(13)     VALUE
                             NC"＜　　　エラー情報　　　＞".
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(01)     VALUE  NC"制".
         05  FILLER          PIC  X(01)     VALUE  ".".
         05  FILLER          PIC  N(03)     VALUE  NC"取引先".
         05  FILLER          PIC  X(33)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"納品日".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(01)     VALUE  NC"商".
         05  FILLER          PIC  X(01)     VALUE  "･".
         05  FILLER          PIC  N(01)     VALUE  NC"伝".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(03)     VALUE  NC"商品名".
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  X(03)     VALUE  "(  ".
         05  FILLER          PIC  N(07)
                                  VALUE NC"サカタ商品情報".
         05  FILLER          PIC  X(04)     VALUE  "   )".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(10)
                             VALUE NC"__________".
         05  FILLER          PIC  N(06)
                             VALUE NC"______".
*
 01  MS01.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-RENBAN     PIC  X(05).
         05  FILLER          PIC  X(06)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(09).
         05  MS01-DENNOX     REDEFINES  MS01-DENNO.
             07 MS01-DENX    PIC  X(02).
         05  MS01-KUGIRI     PIC  X(01).
         05  MS01-GYO        PIC  Z9.
         05  MS01-GYOX       REDEFINES  MS01-GYO.
             07 MS01-GYOUX   PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-DENKU      PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TENCD      PIC  ZZZZ9.
         05  MS01-TENCDX     REDEFINES  MS01-TENCD.
             07 MS01-TENX    PIC  X(05).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SBASYO     PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  "･".
         05  MS01-DBASYO     PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-HDATE      PIC  9(08).
         05  MS01-HACCYUUBI  REDEFINES  MS01-HDATE.
             07 MS01-HDATEX  PIC  X(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BUNRUI     PIC  X(04).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-AITESYOCD  PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SURYO      PIC  Z,ZZZ,ZZ9.
         05  MS01-SUURYOUX   REDEFINES  MS01-SURYO.
             07 MS01-SURYOX  PIC  X(09).
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  MS01-GTANKA     PIC  Z,ZZZ,ZZ9.
         05  MS01-GENTANKAX  REDEFINES  MS01-GTANKA.
             07 MS01-GTANKAX PIC  X(09).
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS01-BTANKA     PIC  Z,ZZZ,ZZ9.
         05  MS01-BAITANKAX  REDEFINES  MS01-BTANKA.
             07 MS01-BTANKAX PIC  X(09).
         05  FILLER          PIC  X(05)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  MS01-ERR1       PIC  N(01).
         05  MS01-ERR2       PIC  N(01).
         05  MS01-ERR3       PIC  N(01).
         05  MS01-ERR4       PIC  N(01).
         05  MS01-ERR5       PIC  N(01).
         05  MS01-ERR6       PIC  N(01).
         05  MS01-ERR7       PIC  N(01).
         05  MS01-ERR8       PIC  N(01).
         05  MS01-ERR9       PIC  N(01).
         05  MS01-ERR10      PIC  N(01).
         05  MS01-ERR11      PIC  N(01).
         05  MS01-ERR12      PIC  N(01).
         05  MS01-ERR13      PIC  N(01).
         05  MS01-ERR14      PIC  N(01).
         05  MS01-ERR15      PIC  N(01).
         05  MS01-ERR16      PIC  N(01).
*
 01  MS02.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SEIGYO     PIC  X(01).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-TORICD     PIC  9(08).
         05  MS02-TORIX      REDEFINES  MS02-TORICD.
             07 MS02-TORICDX PIC  X(08).
         05  FILLER          PIC  X(32)     VALUE  SPACE.
         05  MS02-NDATE      PIC  9(08).
         05  MS02-NDATX      REDEFINES  MS02-NDATE.
             07 MS02-NDATEX  PIC  X(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SKU        PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  "･".
         05  MS02-DKU        PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SYOHINMEI  PIC  X(30).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  "(".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SYOCD      PIC  X(19).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(01)     VALUE  ")".
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
             NC"＃　　　　取込データが０件です　！！　　　＃".
 01  LST-DATA-Z.
     03  FILLER         CHARACTER TYPE YB-21.
         05  FILLER          PIC  X(25)     VALUE  SPACE.
         05  FILLER          PIC  N(22)     VALUE
             NC"＃　　　　　　　　　　　　　　　　　　　　＃".
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
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
 01  PARA-IN-EXCEL         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-EXCEL.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DEXXXXL1.
     MOVE      "DEXXXXL1"   TO   AB-FILE.
     MOVE      EXL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
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
 FILEERR-SEC3           SECTION.
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
     OPEN     INPUT     DEXXXXL1
                        TANMS1.
     OPEN     OUTPUT    PRTF.
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
*伝票EXCEL取込ファイルスタート
     PERFORM  DEXXXXL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　エラーデータ　なし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*伝票EXCEL取込ファイル読込
     PERFORM DEXXXXL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　エラーデータ　なし　＃＃"  UPON CONS
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
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    伝票EXCEL取込ファイルスタート
****************************************************************
 DEXXXXL1-START-SEC          SECTION.
*
     MOVE    "DEXXXXL1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   EXL-REC.
     INITIALIZE                        EXL-REC.
*
     MOVE     "1"                 TO   EXL-F301.
     MOVE     ZERO                TO   EXL-F201.
*
     START  DEXXXXL1  KEY  IS  >=      EXL-F301 EXL-F201
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 DEXXXXL1-START-EXIT.
     EXIT.
*
****************************************************************
*    伝票EXCEL取込ファイル読込
****************************************************************
 DEXXXXL1-READ-SEC           SECTION.
*
     MOVE    "DEXXXXL1-READ-SEC"   TO   S-NAME.
*
     READ     DEXXXXL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   DEXXXXL1-READ-EXIT
     END-READ.
*エラー区分チェック
     IF       EXL-F301  NOT =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   DEXXXXL1-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                   TO   DEXXXXL1-READ-CNT.
*
 DEXXXXL1-READ-EXIT.
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
     PERFORM  DEXXXXL1-READ-SEC.
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
     MOVE     PAGE-CNT            TO   HD0-PCNT.
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD0-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD0-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD0-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD0-HH.
     MOVE     WK-TIME-HM(3:2)     TO   HD0-SS.
     MOVE     WK-TIME-HM(5:2)     TO   HD0-MS.
*    取込モード　
     EVALUATE  PARA-IN-EXCEL
       WHEN    "1"
        MOVE   NC"【　伝票入力形式＿伝票番号指定　】"
                                                  TO HD00-EXCEL
       WHEN    "2"
        MOVE   NC"【　伝票入力形式＿伝票自動採番　】"
                                                  TO HD00-EXCEL
       WHEN    "3"
        MOVE   NC"【　量販店入力形式＿伝票自動採番　】"
                                                  TO HD00-EXCEL
     END-EVALUATE.
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD000-TRBUMON.
     MOVE     PARA-IN-TANCD       TO   TAN-F02 HD000-TRTANTO.
     PERFORM  TANMS1-READ-SEC.
     IF  TANMS1-INV-FLG = SPACE
               MOVE  TAN-F03      TO   HD000-TRTANNM
     ELSE
               MOVE  ALL NC"？"   TO   HD000-TRTANNM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD0   AFTER  2.
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     WRITE    PRT-REC       FROM  HD000 AFTER  1.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     MOVE     SPACE               TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     10                  TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF       LINE-CNT  >   50
              PERFORM HEAD-WT-SEC
     END-IF.
*
*明細１行目
*   連番
     IF       EXL-F201  NOT NUMERIC
              MOVE  "?????"       TO   MS01-RENBAN
     ELSE
              MOVE  EXL-F201      TO   MS01-RENBAN
     END-IF.
*   伝票番号
     MOVE     EXL-F205            TO   MS01-DENNO.
*   行
     MOVE     "-"                 TO   MS01-KUGIRI.
     MOVE     EXL-F206            TO   MS01-GYO.
*   伝区
     MOVE     EXL-F207            TO   MS01-DENKU.
*   店舗　　　
     MOVE     EXL-F208            TO   MS01-TENCD.
*   出場　
     MOVE     EXL-F209            TO   MS01-SBASYO.
*   伝場
     MOVE     EXL-F210            TO   MS01-DBASYO.
*   発注日
     IF       EXL-F211  NOT NUMERIC
              MOVE  "?????"       TO   MS01-HDATEX
     ELSE
              MOVE  EXL-F211      TO   MS01-HDATE
     END-IF.
*   分類
     MOVE     EXL-F213            TO   MS01-BUNRUI.
*   相手商品
     MOVE     EXL-F216            TO   MS01-AITESYOCD.
*   数量
     MOVE     "?????????"         TO   MS01-SURYOX.
     IF       EXL-F219  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-SURYOX
              ELSE
                    MOVE    EXL-F219      TO   MS01-SURYO
     END-IF.
*   原価単価
     MOVE     "?????????"         TO   MS01-GTANKAX.
     IF       EXL-F220  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-GTANKAX
              ELSE
                    MOVE    EXL-F220      TO   MS01-GTANKA
     END-IF.
*   売価単価
     MOVE     "?????????"         TO   MS01-BTANKAX.
     IF       EXL-F221  NOT NUMERIC
                    MOVE    "?????????"   TO   MS01-BTANKAX
              ELSE
                    MOVE    EXL-F221      TO   MS01-BTANKA
     END-IF.
*   エラー区分１
     IF  EXL-F302 = " "
              MOVE SPACE          TO   MS01-ERR1
     ELSE
              MOVE NC"×"         TO   MS01-ERR1
     END-IF.
*    エラー区分２
     IF  EXL-F303 = " "
              MOVE SPACE          TO   MS01-ERR2
     ELSE
              MOVE NC"×"         TO   MS01-ERR2
     END-IF.
*    エラー区分３
     IF  EXL-F304 = " "
              MOVE SPACE          TO   MS01-ERR3
     ELSE
              MOVE NC"×"         TO   MS01-ERR3
     END-IF.
*    エラー区分４
     IF  EXL-F305 = " "
              MOVE SPACE          TO   MS01-ERR4
     ELSE
              MOVE NC"×"         TO   MS01-ERR4
     END-IF.
*    エラー区分５
     IF  EXL-F306 = " "
              MOVE SPACE          TO   MS01-ERR5
     ELSE
              MOVE NC"×"         TO   MS01-ERR5
     END-IF.
*    エラー区分６
     IF  EXL-F307 = " "
              MOVE SPACE          TO   MS01-ERR6
     ELSE
              MOVE NC"×"         TO   MS01-ERR6
     END-IF.
*    エラー区分７
     IF  EXL-F308 = " "
              MOVE SPACE          TO   MS01-ERR7
     ELSE
              MOVE NC"×"         TO   MS01-ERR7
     END-IF.
*    エラー区分８
     IF  EXL-F309 = " "
              MOVE SPACE          TO   MS01-ERR8
     ELSE
              MOVE NC"×"         TO   MS01-ERR8
     END-IF.
*    エラー区分９
     IF  EXL-F310 = " "
              MOVE SPACE          TO   MS01-ERR9
     ELSE
              MOVE NC"×"         TO   MS01-ERR9
     END-IF.
*    エラー区分１０
     IF  EXL-F311 = " "
              MOVE SPACE          TO   MS01-ERR10
     ELSE
              MOVE NC"×"         TO   MS01-ERR10
     END-IF.
*    エラー区分１１
     IF  EXL-F312 = " "
              MOVE SPACE          TO   MS01-ERR11
     ELSE
              MOVE NC"×"         TO   MS01-ERR11
     END-IF.
*    エラー区分１２
     IF  EXL-F313 = " "
              MOVE SPACE          TO   MS01-ERR12
     ELSE
              MOVE NC"×"         TO   MS01-ERR12
     END-IF.
*    エラー区分１３
     IF  EXL-F314 = " "
              MOVE SPACE          TO   MS01-ERR13
     ELSE
              MOVE NC"×"         TO   MS01-ERR13
     END-IF.
*    エラー区分１４
     IF  EXL-F315 = " "
              MOVE SPACE          TO   MS01-ERR14
     ELSE
              MOVE NC"×"         TO   MS01-ERR14
     END-IF.
*    エラー区分１５
     IF  EXL-F316 = " "
              MOVE SPACE          TO   MS01-ERR15
     ELSE
              MOVE NC"×"         TO   MS01-ERR15
     END-IF.
*    エラー区分１６
     IF  EXL-F317 = " "
              MOVE SPACE          TO   MS01-ERR16
     ELSE
              MOVE NC"×"         TO   MS01-ERR16
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*---------------------------------------------------
*明細２行目
*   制御区分
     MOVE     EXL-F203            TO   MS02-SEIGYO.
*   取引先
     MOVE     EXL-F204            TO   MS02-TORICD.
*   納品日
     IF       EXL-F212  NOT NUMERIC
              MOVE  "?????"       TO   MS02-NDATEX
     ELSE
              MOVE  EXL-F212      TO   MS02-NDATE
     END-IF.
*   商・伝
     MOVE     EXL-F214            TO   MS02-SKU.
     MOVE     EXL-F215            TO   MS02-DKU.
*   商品名
     MOVE     EXL-F217            TO   MS02-SYOHINMEI(1:15).
     MOVE     EXL-F218            TO   MS02-SYOHINMEI(16:15).
*   サカタ商品コード
     MOVE     EXL-F223            TO   MS02-SYOCD(1:8).
     MOVE     "-"                 TO   MS02-SYOCD(9:1).
     MOVE     EXL-F224            TO   MS02-SYOCD(10:5).
     MOVE     "-"                 TO   MS02-SYOCD(15:1).
     MOVE     EXL-F225            TO   MS02-SYOCD(16:2).
     MOVE     "-"                 TO   MS02-SYOCD(18:1).
     MOVE     EXL-F226            TO   MS02-SYOCD(19:1).
*    明細印刷
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
*行カウント
     ADD      2                   TO    LINE-CNT.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*伝票EXCEL取込ファイル読込
     DISPLAY "DEXXXXL1 PRINT-CNT = " DEXXXXL1-READ-CNT UPON CONS.
*
     CLOSE     DEXXXXL1  TANMS1  PRTF.
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
