****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　  *
*    モジュール名　　　　：　受信エラーリスト（受領・返品）    *
*    作成日／作成者　　　：　2017/09/14 INOUE                  *
*    流用元　　　　　　　：　SDE0050L
*    処理概要　　　　　　：　取込チェックにてエラーとなった　　*
*                            データをリスト出力する。　　　　　*
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0220L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/09/14.
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
*受領・返品累積ファイル
     SELECT   COMRUIL5  ASSIGN    TO        DA-01-VI-COMRUIL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RUI-F01
                                            RUI-F02
                                            RUI-F03
                                            RUI-F04
                                            RUI-F31
                        FILE  STATUS   IS   RUI-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*    受領・返品累積ファイル
 FD  COMRUIL5           LABEL RECORD   IS   STANDARD.
     COPY     COMRUIL5  OF        XFDLIB
              JOINING   RUI       PREFIX.
*取引先マスタ
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     TOKMS2    OF        XFDLIB
     JOINING  TOK       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SUTE-FLG                PIC  X(03)     VALUE  SPACE.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  COMRUIL5-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TOKMS2-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  FLG-HIT                 PIC  X(03)     VALUE  SPACE.
 01  FLG-FIRST               PIC  X(01)     VALUE  "1".
 01  IX                      PIC  9(02)     VALUE  ZERO.
 01  WK-RUI-F03              PIC  9(08)     VALUE  ZERO.
 01  WK-RUI-F04              PIC  9(10)     VALUE  ZERO.
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
     03  RUI-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SAK-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0220L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0220L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0220L".
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
         05  FILLER          PIC  X(08)     VALUE  "SJR0220L".
         05  FILLER          PIC  X(27)     VALUE  SPACE.
         05  FILLER          PIC  N(17)     VALUE
         NC"＜受信エラーリスト（受領・返品）＞".
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
*01  HD00.
*    03  FILLER         CHARACTER  TYPE YA.
*        05  FILLER          PIC  X(44)     VALUE  SPACE.
*        05  HD00-EXCEL      PIC  N(18).
*01  HD000.
*    03  FILLER         CHARACTER TYPE YA.
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  FILLER          PIC  N(07)
*                            VALUE  NC"取込取引先　：".
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  HD000-TRBUMON   PIC  X(04).
*        05  FILLER          PIC  X(01)     VALUE  "-".
*        05  HD000-TRTANTO   PIC  X(02).
*        05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  HD000-TRTANNM   PIC  N(10).
*
 01  HD01.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"エラー情報→".
         05  FILLER          PIC  N(05)     VALUE
             NC"_取引先Ｍ".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_店舗Ｍ".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_計上日".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_原単価".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"_変換ＴＢＬ".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"_－－－".
*
 01  HD02.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
 01  HD03.
     03  FILLER              PIC  X(117)    VALUE  ALL " ".
     03  FILLER              PIC  N(07)     VALUE
                             NC"＜エラー情報＞"
                             CHARACTER TYPE YA.
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)    VALUE SPACE.
         05  FILLER          PIC  N(05)    VALUE NC"バッチ日付".
         05  FILLER          PIC  X(01)    VALUE SPACE.
         05  FILLER          PIC  N(02)    VALUE NC"時刻".
         05  FILLER          PIC  X(02)    VALUE SPACE.
         05  FILLER          PIC  N(03)    VALUE NC"取引先".
         05  FILLER          PIC  X(28)    VALUE SPACE.
         05  FILLER          PIC  N(04)    VALUE NC"伝票番号".
         05  FILLER          PIC  X(04)    VALUE SPACE.
         05  FILLER          PIC  N(05)    VALUE NC"元伝票番号".
         05  FILLER          PIC  X(01)    VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(04)    VALUE NC"相手伝区".
         05  FILLER          PIC  X(01)    VALUE SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(03)     VALUE  NC"店舗Ｃ".
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(03)     VALUE  NC"計上日".
         05  FILLER          PIC  X(04)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE  NC"分類ＣＤ".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  FILLER          PIC  N(02)     VALUE  NC"訂区".
         05  FILLER          PIC  X(02)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(10)
                             VALUE NC"__________".
*
 01  MS01.
     03  FILLER.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BDATE      PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BTIME      PIC  X(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TORICD     PIC  9(08).
         05  MS01-TORICDX    REDEFINES  MS01-TORICD.
             07 MS01-TORIX   PIC  X(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TORINM     PIC  N(16)     CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(10).
         05  MS01-DENNOX     REDEFINES  MS01-DENNO.
             07 MS01-DENX    PIC  X(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-MDENNO     PIC  9(10).
         05  MS01-MDENNOX    REDEFINES  MS01-MDENNO.
             07 MS01-MDENX   PIC  X(10).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-DENKU      PIC  X(02).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-TENCD      PIC  ZZZZ9.
         05  MS01-TENCDX     REDEFINES  MS01-TENCD.
             07 MS01-TENX    PIC  X(05).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-KDATE      PIC  X(10).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-BUNRUI     PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TEISEIH    PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
*
 01  HD05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(12) VALUE SPACE.
         05  FILLER          PIC  N(01) VALUE NC"行".
         05  FILLER          PIC  X(02) VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(02) VALUE NC"赤黒".
         05  FILLER          PIC  X(01) VALUE SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  N(07) VALUE
                                        NC"相手商品コード".
         05  FILLER          PIC  X(01) VALUE SPACE.
         05  FILLER          PIC  N(03) VALUE NC"商品名".
         05  FILLER          PIC  X(35) VALUE SPACE.
         05  FILLER          PIC  N(02) VALUE NC"数量".
         05  FILLER          PIC  X(06) VALUE SPACE.
         05  FILLER          PIC  N(04) VALUE NC"原価単価".
         05  FILLER          PIC  X(02) VALUE SPACE.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  N(04) VALUE NC"返品理由".
         05  FILLER          PIC  X(01) VALUE SPACE.
         05  FILLER          PIC  N(02) VALUE NC"訂区".
*
 01  MS02.
     03  FILLER.
         05  FILLER          PIC  X(12)     VALUE  SPACE.
         05  MS02-GYO        PIC  Z9.
         05  MS02-GYOX       REDEFINES  MS02-GYO.
             07 MS02-GYOXX   PIC  X(02).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-AKAKURO    PIC  N(01)     CHARACTER TYPE YA.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS02-SHOCD      PIC  X(14).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SHONM1     PIC  X(35).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS02-SURYO      PIC  Z,ZZZ,ZZ9.
         05  MS02-SUURYOUX   REDEFINES  MS02-SURYO.
             07 MS02-SURYOX  PIC  X(09).
         05  MS02-GTANKA     PIC  ZZ,ZZZ,ZZ9.99.
         05  MS02-GENTANKAX  REDEFINES  MS02-GTANKA.
             07 MS02-GTANKAX PIC  X(13).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS02-RIYUU      PIC  X(03).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS02-TEISEIM    PIC  X(04).
         05  FILLER          PIC  X(11)     VALUE  SPACE.
     03  FILLER         CHARACTER TYPE YA.
         05  MS02-ERR1       PIC  N(01).
         05  MS02-ERR2       PIC  N(01).
         05  MS02-ERR3       PIC  N(01).
         05  MS02-ERR4       PIC  N(01).
         05  MS02-ERR5       PIC  N(01).
         05  MS02-ERR6       PIC  N(01).
         05  MS02-ERR7       PIC  N(01).
         05  MS02-ERR8       PIC  N(01).
         05  MS02-ERR9       PIC  N(01).
         05  MS02-ERR10      PIC  N(01).
*
 01  MS03.
     03  FILLER.
         05  FILLER          PIC  X(35)     VALUE  SPACE.
         05  MS03-SHONM2     PIC  X(35).
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
             NC"＃　　　エラーデータは０件です　　　　　　＃".
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
 01  PARA-IN-JDATE         PIC   9(08).
 01  PARA-IN-JTIME         PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JDATE
                                       PARA-IN-JTIME.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIL5.
     MOVE      "COMRUIL5"   TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     COMRUIL5
                        TOKMS2.
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
*受領・返品累積ファイルスタート
     PERFORM  COMRUIL5-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　エラーデータなし　＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  HD02        AFTER  1
          WRITE    PRT-REC      FROM  HD03        AFTER  1
          WRITE    PRT-REC      FROM  HD04        AFTER  1
*         バッチ日付
          IF       RUI-F01   NOT NUMERIC
                   MOVE  "??????????"  TO   MS01-BDATE
          ELSE
                   MOVE  RUI-F01(1:4)  TO   MS01-BDATE(1:4)
                   MOVE  "/"           TO   MS01-BDATE(5:1)
                   MOVE  RUI-F01(5:2)  TO   MS01-BDATE(6:2)
                   MOVE  "/"           TO   MS01-BDATE(8:1)
                   MOVE  RUI-F01(7:2)  TO   MS01-BDATE(9:2)
          END-IF
*         バッチ時刻
          IF       RUI-F02   NOT NUMERIC
                   MOVE  "??????????"  TO   MS01-BTIME
          ELSE
                   MOVE  RUI-F02(3:2)  TO   MS01-BTIME(1:2)
                   MOVE  ":"           TO   MS01-BTIME(3:1)
                   MOVE  RUI-F02(5:2)  TO   MS01-BTIME(4:2)
          END-IF
          WRITE    PRT-REC      FROM  MS01        AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*受領・返品累積ファイル読込
     PERFORM COMRUIL5-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　エラーデータなし．＃＃"  UPON CONS
          PERFORM  HEAD-WT-SEC
          WRITE    PRT-REC      FROM  HD02        AFTER  1
          WRITE    PRT-REC      FROM  HD03        AFTER  1
          WRITE    PRT-REC      FROM  HD04        AFTER  1
*         バッチ日付
          IF       RUI-F01   NOT NUMERIC
                   MOVE  "??????????"  TO   MS01-BDATE
          ELSE
                   MOVE  RUI-F01(1:4)  TO   MS01-BDATE(1:4)
                   MOVE  "/"           TO   MS01-BDATE(5:1)
                   MOVE  RUI-F01(5:2)  TO   MS01-BDATE(6:2)
                   MOVE  "/"           TO   MS01-BDATE(8:1)
                   MOVE  RUI-F01(7:2)  TO   MS01-BDATE(9:2)
          END-IF
*         バッチ時刻
          IF       RUI-F02   NOT NUMERIC
                   MOVE  "??????????"  TO   MS01-BTIME
          ELSE
                   MOVE  RUI-F02(3:2)  TO   MS01-BTIME(1:2)
                   MOVE  ":"           TO   MS01-BTIME(3:1)
                   MOVE  RUI-F02(5:2)  TO   MS01-BTIME(4:2)
          END-IF
          WRITE    PRT-REC      FROM  MS01        AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  5
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Y  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-Z  AFTER  1
          WRITE    PRT-REC      FROM  LST-DATA-X  AFTER  1
          MOVE    "END"         TO    END-FLG
          GO                    TO    INIT-EXIT
     END-IF.
*取引先.伝票_保管
     MOVE RUI-F03               TO    WK-RUI-F03.
     MOVE RUI-F04               TO    WK-RUI-F04.
*
*ヘッダ印刷
     PERFORM HEAD-WT-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    受領・返品累積ファイルスタート
****************************************************************
 COMRUIL5-START-SEC          SECTION.
*
     MOVE    "COMRUIL5-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   RUI-REC.
     INITIALIZE                        RUI-REC.
*
     MOVE     PARA-IN-JDATE       TO   RUI-F01.
     MOVE     PARA-IN-JTIME       TO   RUI-F02.
     MOVE     ZERO                TO   RUI-F03.
     MOVE     ZERO                TO   RUI-F04.
     MOVE     ZERO                TO   RUI-F31.
*
     START  COMRUIL5  KEY  IS  >= RUI-F01  RUI-F02
                                  RUI-F03  RUI-F04
                                  RUI-F31
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 COMRUIL5-START-EXIT.
     EXIT.
*
****************************************************************
*    受領・返品累積ファイル読込
****************************************************************
 COMRUIL5-READ-SEC           SECTION.
*
     MOVE    "COMRUIL5-READ-SEC"  TO   S-NAME.
*
     READ     COMRUIL5  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   COMRUIL5-READ-EXIT
     END-READ.
*範囲チェック
     IF     ( RUI-F01   IS NUMERIC       ) AND
            ( RUI-F02   IS NUMERIC       ) AND
            ( RUI-F01   =  PARA-IN-JDATE ) AND
            ( RUI-F02   =  PARA-IN-JTIME )
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   COMRUIL5-READ-EXIT
     END-IF.
*対象チェック
     MOVE    "   "                TO   FLG-HIT.
     PERFORM  VARYING  IX   FROM  1    BY   1    UNTIL  IX  >  10
              IF    RUI-F56(IX:1) =    "1"
                    MOVE "HIT"    TO   FLG-HIT
              END-IF
     END-PERFORM.
     IF       FLG-HIT   =   "   "
              GO                  TO   COMRUIL5-READ-SEC
     END-IF.
*件数カウント
     ADD      1                   TO   COMRUIL5-READ-CNT.
*
 COMRUIL5-READ-EXIT.
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
     PERFORM  COMRUIL5-READ-SEC.
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
*    取引先名取得
*    MOVE     PARA-IN-JDATE       TO   TOK-F01 HD000-TRBUMON.
*    MOVE     PARA-IN-JTIME       TO   TOK-F02 HD000-TRTANTO.
*    PERFORM  TOKMS2-READ-SEC.
*    IF  TOKMS2-INV-FLG = SPACE
*              MOVE  TOK-F03      TO   HD000-TRTANNM
*    ELSE
*              MOVE  ALL NC"？"   TO   HD000-TRTANNM
*    END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD0   AFTER  2.
     WRITE    PRT-REC       FROM  HD01  AFTER  3.
*    MOVE     SPACE               TO    PRT-REC.
*    WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     5                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
 MEISAI-WT-01.
*   改頁判定
     IF       LINE-CNT  >=  49
              PERFORM HEAD-WT-SEC
              MOVE  "1"           TO   FLG-FIRST
     END-IF.
*
 MEISAI-WT-02.
*明細１行目
     IF       FLG-FIRST           =    "1"
              MOVE  " "           TO   FLG-FIRST
              GO                  TO   MEISAI-WT-02-1
     END-IF.
     IF     ( RUI-F03             =    WK-RUI-F03 ) AND
            ( RUI-F04             =    WK-RUI-F04 )
              GO                  TO   MEISAI-WT-03
     END-IF.
 MEISAI-WT-02-1.
*明細１行目
     IF       LINE-CNT  >   43
              PERFORM HEAD-WT-SEC
     END-IF.
*
*   バッチ日付
     IF       RUI-F01   NOT NUMERIC
              MOVE  "??????????"  TO   MS01-BDATE
     ELSE
              MOVE  RUI-F01(1:4)  TO   MS01-BDATE(1:4)
              MOVE  "/"           TO   MS01-BDATE(5:1)
              MOVE  RUI-F01(5:2)  TO   MS01-BDATE(6:2)
              MOVE  "/"           TO   MS01-BDATE(8:1)
              MOVE  RUI-F01(7:2)  TO   MS01-BDATE(9:2)
     END-IF.
*   バッチ時刻
     IF       RUI-F02   NOT NUMERIC
              MOVE  "??????????"  TO   MS01-BTIME
     ELSE
              MOVE  RUI-F02(3:2)  TO   MS01-BTIME(1:2)
              MOVE  ":"           TO   MS01-BTIME(3:1)
              MOVE  RUI-F02(5:2)  TO   MS01-BTIME(4:2)
     END-IF.
*    取引先
     MOVE     WK-RUI-F03          TO   TOK-F01 MS01-TORICD.
     PERFORM  TOKMS2-READ-SEC.
     IF       TOKMS2-INV-FLG = SPACE
               MOVE  TOK-F02      TO   MS01-TORINM
     ELSE
               MOVE  ALL NC"？"   TO   MS01-TORINM
     END-IF.
*   伝票番号
     MOVE     RUI-F04             TO   MS01-DENNO.
*   元伝票番号
     MOVE     RUI-F05             TO   MS01-MDENNO.
*   相手伝区
     MOVE     RUI-F06             TO   MS01-DENKU.
*   店舗Ｃ　　
     MOVE     RUI-F07             TO   MS01-TENCD.
*   計上日
     IF       RUI-F08   NOT NUMERIC
              MOVE  "??????????"  TO   MS01-KDATE
     ELSE
              MOVE  RUI-F08(1:4)  TO   MS01-KDATE(1:4)
              MOVE  "/"           TO   MS01-KDATE(5:1)
              MOVE  RUI-F08(5:2)  TO   MS01-KDATE(6:2)
              MOVE  "/"           TO   MS01-KDATE(8:1)
              MOVE  RUI-F08(7:2)  TO   MS01-KDATE(9:2)
     END-IF.
*   分類ＣＤ
     MOVE     RUI-F09             TO   MS01-BUNRUI.
*   訂区H
     MOVE     RUI-F10             TO   MS01-TEISEIH.
*   明細印刷
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  2.
*行カウント
     ADD      6                   TO    LINE-CNT.
*取引先.伝票_退避
     MOVE     RUI-F03             TO    WK-RUI-F03.
     MOVE     RUI-F04             TO    WK-RUI-F04.
*---------------------------------------------------
 MEISAI-WT-03.
*明細２行目
*   行
     MOVE     RUI-F31             TO   MS02-GYO.
*   赤黒
     IF       RUI-F32   =  "1"
              MOVE  NC"赤"        TO   MS02-AKAKURO
     ELSE
              MOVE  NC"　"        TO   MS02-AKAKURO
     END-IF.
*   相手商品コード
     MOVE     RUI-F33             TO   MS02-SHOCD.
*   商品名１
     MOVE     RUI-F37             TO   MS02-SHONM1.
*   数量
     MOVE     "?????????"         TO   MS02-SURYOX.
     IF       RUI-F34   NOT NUMERIC
              MOVE    "?????????" TO   MS02-SURYOX
              ELSE
                    MOVE    RUI-F34       TO   MS02-SURYO
     END-IF.
*   原価単価
     MOVE     "?????????"         TO   MS02-GTANKAX.
     IF       RUI-F35   NOT NUMERIC
                    MOVE    "?????????"   TO   MS02-GTANKAX
              ELSE
                    MOVE    RUI-F35       TO   MS02-GTANKA
     END-IF.
*   返品理由
     MOVE     RUI-F39             TO   MS02-RIYUU.
*   訂区M
     MOVE     RUI-F40             TO   MS02-TEISEIM.
*   エラー区分１
     IF  RUI-F561 = " "
              MOVE SPACE          TO   MS02-ERR1
     ELSE
              MOVE NC"×"         TO   MS02-ERR1
     END-IF.
*    エラー区分２
     IF  RUI-F562 = " "
              MOVE SPACE          TO   MS02-ERR2
     ELSE
              MOVE NC"×"         TO   MS02-ERR2
     END-IF.
*    エラー区分３
     IF  RUI-F563 = " "
              MOVE SPACE          TO   MS02-ERR3
     ELSE
              MOVE NC"×"         TO   MS02-ERR3
     END-IF.
*    エラー区分４
     IF  RUI-F564 = " "
              MOVE SPACE          TO   MS02-ERR4
     ELSE
              MOVE NC"×"         TO   MS02-ERR4
     END-IF.
*    エラー区分５
     IF  RUI-F565 = " "
              MOVE SPACE          TO   MS02-ERR5
     ELSE
              MOVE NC"×"         TO   MS02-ERR5
     END-IF.
*    エラー区分６
     IF  RUI-F566 = " "
              MOVE SPACE          TO   MS02-ERR6
     ELSE
              MOVE NC"×"         TO   MS02-ERR6
     END-IF.
*    エラー区分７
     IF  RUI-F567 = " "
              MOVE SPACE          TO   MS02-ERR7
     ELSE
              MOVE NC"×"         TO   MS02-ERR7
     END-IF.
*    エラー区分８
     IF  RUI-F568 = " "
              MOVE SPACE          TO   MS02-ERR8
     ELSE
              MOVE NC"×"         TO   MS02-ERR8
     END-IF.
*    エラー区分９
     IF  RUI-F569 = " "
              MOVE SPACE          TO   MS02-ERR9
     ELSE
              MOVE NC"×"         TO   MS02-ERR9
     END-IF.
*    エラー区分１０
     IF  RUI-F56A = " "
              MOVE SPACE          TO   MS02-ERR10
     ELSE
              MOVE NC"×"         TO   MS02-ERR10
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT.
*
*---------------------------------------------------
 MEISAI-WT-04.
*明細３行目
*   商品名２
     MOVE     RUI-F38             TO   MS03-SHONM2.
*    明細印刷
     WRITE    PRT-REC       FROM  MS03  AFTER  1.
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
*
     IF        LINE-CNT  >       1
               WRITE     PRT-REC FROM  HD02  AFTER  1
     END-IF.
*
     DISPLAY   "明細出力件数 = " COMRUIL5-READ-CNT UPON CONS.
*
     CLOSE     COMRUIL5  TOKMS2  PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ索引
****************************************************************
 TOKMS2-READ-SEC           SECTION.
     MOVE "TOKMS2-READ-SEC"       TO   S-NAME.
*
     READ       TOKMS2
         INVALID       MOVE "INV" TO   TOKMS2-INV-FLG
         NOT  INVALID  MOVE SPACE TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
