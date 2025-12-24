****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　伝票ＥＸＣＥＬ連携　　            *
*    モジュール名　　　　：　伝票番号採番リスト発行　　　　　　*
*    作成日／作成者　　　：　2016/09/30 TAKAHASHI              *
*    処理概要　　　　　　：　伝票番号採番リストデータを読み　　*
*                            伝票番号採番リストを発行する。　　*
*    更新日／更新者　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SDE0090L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2016/09/30.
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
*伝票番号採番リストデータ
     SELECT   LSTXXXL1  ASSIGN    TO       DA-01-VI-LSTXXXL1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     SEQUENTIAL
                        RECORD    KEY      LST-F01  LST-F02
                                           LST-F03  LST-F05
                        FILE  STATUS   IS  LST-STATUS.
*担当者マスタ
     SELECT   HTANMS    ASSIGN    TO       DA-01-VI-TANMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TAN-F01  TAN-F02
                        FILE      STATUS   TAN-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO       DA-01-VI-TOKMS2
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TOK-F01
                        FILE      STATUS   TOK-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO       DA-01-VI-TENMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TEN-F52  TEN-F011
                        FILE      STATUS   TEN-STATUS.
*条件ファイル（伝票区分名を取得）
     SELECT   HJYOKEN   ASSIGN    TO       DA-01-VI-JYOKEN1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      JYO-F01  JYO-F02
                        FILE      STATUS   JYO-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*伝票番号採番リストデータ
 FD  LSTXXXL1            LABEL RECORD   IS   STANDARD.
     COPY     LSTXXXF   OF        XFDLIB
     JOINING  LST       AS        PREFIX.
*担当者マスタ
 FD  HTANMS             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*取引先スタ
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
     JOINING  TOK       AS        PREFIX.
*店舗マスタ
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
     JOINING  TEN       AS        PREFIX.
*条件ファイル
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
     JOINING  JYO       AS        PREFIX.
*プリンタ
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  PAGE-CNT                PIC  9(04)     VALUE  ZERO.
 01  LINE-CNT                PIC  9(02)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  MEIS-CNT                PIC  9(07)     VALUE  ZERO.
 01  HTANMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  IX                      PIC  9(01)     VALUE  ZERO.
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
     03  LST-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SDE0090L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0090L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SDE0090L".
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
     03  FILLER              PIC  X(116)    VALUE  SPACE.
     03  FILLER              PIC  X(19)     VALUE
         "+-----+-----+-----+".
 01  HD01.
     03  FILLER         CHARACTER  TYPE YB-21.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(08)     VALUE  "SDE0090L".
         05  FILLER          PIC  X(16)     VALUE  SPACE.
         05  FILLER          PIC  N(20)     VALUE
         NC"＜（伝票ＥＸＣＥＬ）伝票番号採番リスト＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(10)     VALUE  SPACE.
         05  HD01-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)     VALUE  NC"年".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"月".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  N(01)     VALUE  NC"日".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)     VALUE  NC"頁".
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  X(19)     VALUE
             "!     !     !     !".
 01  HD02.
     03  FILLER              PIC  X(095)    VALUE  SPACE.
     03  HD02-JTIME          PIC  X(08).
     03  FILLER              PIC  X(13)     VALUE  SPACE.
     03  FILLER              PIC  X(19)     VALUE
         "!     !     !     !".
*
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(74)     VALUE  SPACE.
         05  FILLER          PIC  N(14)     VALUE
             NC"備→備考区分　　１：備考行有".
         05  FILLER          PIC  X(14)     VALUE  SPACE.
         05  FILLER          PIC  X(19)     VALUE
             "!     !     !     !".
*
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"変換担当者：".
         05  HD04-TANCD      PIC  X(07).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD04-TANNM      PIC  N(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"変換日時：".
         05  HD04-HENDATE    PIC  X(10).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD04-HENTIME    PIC  X(08).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(14)     VALUE
             NC"エ→エラー区分　１：エラー有".
         05  FILLER          PIC  X(14)     VALUE  SPACE.
         05  FILLER          PIC  X(19)     VALUE
             "!     !     !     !".
*
 01  HD05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  FILLER          PIC  N(05)     VALUE
             NC"伝票区分：".
         05  HD05-DENKU      PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD05-DENNM      PIC  N(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(04)     VALUE
             NC"取引先：".
         05  HD05-TOKCD      PIC  9(08).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD05-TOKNM      PIC  N(10).
         05  FILLER          PIC  X(12)     VALUE  SPACE.
         05  FILLER          PIC  N(16)     VALUE
             NC"採→採番区分　　１：伝票番号指定".
         05  FILLER          PIC  X(10)     VALUE  SPACE.
         05  FILLER          PIC  X(19)     VALUE
             "+-----+-----+-----+".
*
 01  HD06.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(05)    VALUE
             NC"店舗情報".
         05  FILLER          PIC  X(17)    VALUE  SPACE.
         05  FILLER          PIC  N(04)    VALUE
             NC"伝票番号".
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"行".
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(03)    VALUE
             NC"納品日".
         05  FILLER          PIC  X(05)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"備".
         05  FILLER          PIC  X(01)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"エ".
         05  FILLER          PIC  X(01)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"採".
         05  FILLER          PIC  X(08)    VALUE  SPACE.
         05  FILLER          PIC  N(05)    VALUE
             NC"店舗情報".
         05  FILLER          PIC  X(17)    VALUE  SPACE.
         05  FILLER          PIC  N(04)    VALUE
             NC"伝票番号".
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"行".
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(03)    VALUE
             NC"納品日".
         05  FILLER          PIC  X(05)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"備".
         05  FILLER          PIC  X(01)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"エ".
         05  FILLER          PIC  X(01)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"採".
*
 01  SEN.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
*
 01  MS01.
     03  FILLER              PIC  X(03)     VALUE  SPACE.
     03  MEISAI              OCCURS  2.
         05  MS01-TENCD      PIC  9(05).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TENNM      PIC  N(10)
             CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-DENNO      PIC  9(09).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-GYO        PIC  Z9.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-MNOUDT     PIC  X(10).
         05  FILLER          PIC  X(03)     VALUE  SPACE.
         05  MS01-BI         PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-ERR        PIC  X(01).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-SAIBAN     PIC  X(01).
         05  FILLER          PIC  X(05)     VALUE  SPACE.
         05  MS01-KU         PIC  N(01)
             CHARACTER TYPE YA.
         05  FILLER          PIC  X(02)     VALUE  SPACE.
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*編集
 01  WK-MS01-MNOUDT     PIC  X(10)     VALUE  SPACE.
*
*ブレイクキー
 01  WK-B-KEY.
     03  WK-B-TOKCD     PIC  9(08)     VALUE  ZERO.
     03  WK-B-DENKU     PIC  X(02)     VALUE  SPACE.
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
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LSTXXXL1.
     MOVE      "LSTXXXL1"   TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTANMS.
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
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS1  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1 "   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
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
     OPEN     INPUT     LSTXXXL1  HTANMS  HTOKMS  HTENMS
                        HJYOKEN
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
******************
*システム時刻編集*
******************
     ACCEPT   SYS-TIME          FROM   TIME.
*初期値セット
     MOVE     ZERO              TO   IX.
*伝票番号採番リストデータ読込
     PERFORM LSTXXXL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃出力対象データ無！＃＃"  UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    伝票番号採番リストデータ読込
****************************************************************
 LSTXXXL1-READ-SEC           SECTION.
*
     MOVE    "LSTXXXL1-READ-SEC"   TO   S-NAME.
*
     READ     LSTXXXL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   LSTXXXL1-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   READ-CNT.
*
 LSTXXXL1-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*取引先ＣＤ／伝票区分ブレイクチェック
     IF       LST-F01  NOT =  WK-B-TOKCD
     OR       LST-F02  NOT =  WK-B-DENKU
              IF   IX  =  1
                   PERFORM  MEISAI-WT-SEC
              END-IF
              PERFORM  HEAD-WT-SEC
              MOVE LST-F01       TO   WK-B-TOKCD
              MOVE LST-F02       TO   WK-B-DENKU
              MOVE ZERO          TO   IX
              MOVE SPACE         TO   MS01
     END-IF.
*明細行初期化
     IF  IX  =  ZERO
         MOVE   SPACE            TO   MS01
     END-IF.
*
     ADD     1                   TO    IX.
*****店舗ＣＤ
     MOVE    LST-F03             TO    MS01-TENCD(IX).
*****店舗名称取得
     MOVE    LST-F01             TO    TEN-F52.
     MOVE    LST-F03             TO    TEN-F011.
     PERFORM  HTENMS-READ-SEC.
     IF   HTENMS-INV-FLG  =  "INV"
          MOVE   NC"？"          TO    MS01-TENNM(IX)
     ELSE
          MOVE   TEN-F02         TO    MS01-TENNM(IX)
     END-IF.
*****伝票番号
     MOVE    LST-F05             TO    MS01-DENNO(IX).
*****行数
     MOVE    LST-F06             TO    MS01-GYO(IX).
*****納品日
     MOVE    LST-F04(1:4)        TO    WK-MS01-MNOUDT(1:4).
     MOVE    "/"                 TO    WK-MS01-MNOUDT(5:1).
     MOVE    LST-F04(5:2)        TO    WK-MS01-MNOUDT(6:2).
     MOVE    "/"                 TO    WK-MS01-MNOUDT(8:1).
     MOVE    LST-F04(7:2)        TO    WK-MS01-MNOUDT(9:2).
     MOVE    WK-MS01-MNOUDT      TO    MS01-MNOUDT(IX).
*****備考区分
     MOVE    LST-F93             TO    MS01-BI(IX).
*****エラー
     MOVE    LST-F94             TO    MS01-ERR(IX).
*****採番区分
     MOVE    LST-F95             TO    MS01-SAIBAN(IX).
*****区切り出力
     IF   IX  =  1
          MOVE  NC"｜"           TO    MS01-KU(IX)
     END-IF.
*****明細出力判定
     IF   IX  =  2
          PERFORM  MEISAI-WT-SEC
     END-IF.
*伝票番号採番リストデータ読込
     PERFORM LSTXXXL1-READ-SEC.
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
*    システム日付セット
     MOVE     SYS-DATEW(1:4)      TO   HD01-YYYY.
     MOVE     SYS-DATEW(5:2)      TO   HD01-MM.
     MOVE     SYS-DATEW(7:2)      TO   HD01-DD.
*    システム時刻セット
     MOVE     WK-TIME-HM(1:2)     TO   HD02-JTIME(1:2).
     MOVE     ":"                 TO   HD02-JTIME(3:1).
     MOVE     WK-TIME-HM(3:2)     TO   HD02-JTIME(4:2).
     MOVE     ":"                 TO   HD02-JTIME(6:1).
     MOVE     WK-TIME-HM(5:2)     TO   HD02-JTIME(7:2).
*    担当者名取得
     MOVE     LST-F98             TO   TAN-F01 HD04-TANCD(1:4).
     MOVE     LST-F99             TO   TAN-F02 HD04-TANCD(6:2).
     MOVE     "-"                 TO   HD04-TANCD(5:1).
     PERFORM  HTANMS-READ-SEC.
     IF  HTANMS-INV-FLG = SPACE
               MOVE TAN-F03    TO   HD04-TANNM
     ELSE
               MOVE ALL NC"？" TO   HD04-TANNM
     END-IF.
*    変換日付
     MOVE     LST-F96(1:4)     TO   HD04-HENDATE(1:4).
     MOVE     "/"              TO   HD04-HENDATE(5:1).
     MOVE     LST-F96(5:2)     TO   HD04-HENDATE(6:2).
     MOVE     "/"              TO   HD04-HENDATE(8:1).
     MOVE     LST-F96(7:2)     TO   HD04-HENDATE(9:2).
*    システム時刻セット
     MOVE     LST-F97(1:2)     TO   HD04-HENTIME(1:2).
     MOVE     ":"              TO   HD04-HENTIME(3:1).
     MOVE     LST-F97(3:2)     TO   HD04-HENTIME(4:2).
     MOVE     ":"              TO   HD04-HENTIME(6:1).
     MOVE     LST-F97(5:2)     TO   HD04-HENTIME(7:2).
*    伝票区分取得
     MOVE     01                  TO   JYO-F01.
     MOVE     LST-F02             TO   JYO-F02 HD05-DENKU.
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = SPACE
               MOVE JYO-F03    TO   HD05-DENNM
     ELSE
               MOVE ALL NC"？" TO   HD05-DENNM
     END-IF.
*    取引先名取得
     MOVE     LST-F01             TO   TOK-F01 HD05-TOKCD.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG = SPACE
               MOVE TOK-F02    TO   HD05-TOKNM
     ELSE
               MOVE ALL NC"？" TO   HD05-TOKNM
     END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD00  AFTER  1.
     WRITE    PRT-REC       FROM  HD01  AFTER  1.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  HD05  AFTER  1.
     WRITE    PRT-REC       FROM  SEN   AFTER  1.
     WRITE    PRT-REC       FROM  HD06  AFTER  1.
     WRITE    PRT-REC       FROM  SEN   AFTER  1.
*行カウントアップ
     MOVE     9                   TO    LINE-CNT.
*
 HEAD-WT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    明細印字
*--------------------------------------------------------------*
 MEISAI-WT-SEC          SECTION.
*
     MOVE    "MEISAI-WT-SEC"      TO   S-NAME.
*   改頁判定
     IF       LINE-CNT  >   55
              PERFORM HEAD-WT-SEC
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
*行カウント
     ADD      1                   TO    LINE-CNT  MEIS-CNT.
*添字クリア
     MOVE     ZERO                TO    IX.
*
 MEISAI-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*明細印字判定
     IF  IX  >=  1
         PERFORM  MEISAI-WT-SEC
     END-IF.
*
     DISPLAY NC"＃読込件数" " = " READ-CNT  UPON CONS.
     DISPLAY NC"＃明細件数" " = " MEIS-CNT  UPON CONS.
     DISPLAY NC"＃出力枚数" " = " PAGE-CNT  UPON CONS.
*ファイルＣＬＯＳＥ
     CLOSE     LSTXXXL1 HTANMS  HTOKMS  HTENMS  HJYOKEN  PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 HTANMS-READ-SEC            SECTION.
*
     MOVE     "HTANMS-READ-SEC"  TO  S-NAME.
*
     READ     HTANMS
       INVALID
              MOVE "INV"     TO   HTANMS-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   HTANMS-INV-FLG
     END-READ.
*
 HTANMS-READ-EXIT.
     EXIT.
****************************************************************
*    取引先マスタ索引
****************************************************************
 HTOKMS-READ-SEC            SECTION.
*
     MOVE     "HTOKMS-READ-SEC"  TO  S-NAME.
*
     READ     HTOKMS
       INVALID
              MOVE "INV"     TO   HTOKMS-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*    店舗マスタ索引
****************************************************************
 HTENMS-READ-SEC            SECTION.
*
     MOVE     "HTENMS-READ-SEC"  TO  S-NAME.
*
     READ     HTENMS
       INVALID
              MOVE "INV"     TO   HTENMS-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*    条件ファイル索引
****************************************************************
 HJYOKEN-READ-SEC           SECTION.
*
     MOVE     "HJYOKEN-READ-SEC" TO  S-NAME.
*
     READ     HJYOKEN
       INVALID
              MOVE "INV"     TO   HJYOKEN-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
