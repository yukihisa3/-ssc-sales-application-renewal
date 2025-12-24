****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　棚卸　　　　　　　　　            *
*    モジュール名　　　　：　棚卸実績リスト出力　　　　　　　　*
*    作成日／作成者　　　：　2018/12/27 INOUE                  *
*    処理概要　　　　　　：　検品システムから取り込んだ　　　　*
*                            確定データより棚卸実績リストを　　*
*                            出力する　　　　　　　　　　　　　*
*    更新日／更新者　　　：　2019/01/07 INOUE                  *
*    更新概要　　　　　　：　差数を符号付きに変更　　　　　　　*
*    更新日／更新者　　　：　2019/02/01 INOUE                  *
*    更新概要　　　　　　：　棚卸確定データレイアウト変更　　　*
*                            　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0440L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/12/27.
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
*棚卸確定データ
     SELECT   RCVTANXX  ASSIGN    TO       DA-01-S-RCVTANXX
                        ORGANIZATION       SEQUENTIAL
                        ACCESS    MODE     SEQUENTIAL
                        FILE  STATUS   IS  RCV-STATUS.
*担当者マスタ
     SELECT   TANMS1    ASSIGN    TO       DA-01-VI-TANMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      TAN-F01  TAN-F02
                        FILE      STATUS   TAN-STATUS.
*倉庫マスタ
     SELECT   ZSOKMS1   ASSIGN    TO       DA-01-VI-ZSOKMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      SOK-F01
                        FILE      STATUS   SOK-STATUS.
*名称マスタ
     SELECT   MEIMS1    ASSIGN    TO       DA-01-VI-MEIMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      MEI-F011  MEI-F0121
                                           MEI-F0122 MEI-F0123
                        FILE      STATUS   MEI-STATUS.
*プリンタ
     SELECT   PRTF      ASSIGN         LP-04-PRTF
                        FILE  STATUS   IS   PRT-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*棚卸確定データ
 FD  RCVTANXX           LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 30   RECORDS.
     COPY     RCVTANXX  OF        XFDLIB
     JOINING  RCV       AS        PREFIX.
*担当者マスタ
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
     JOINING  TAN       AS        PREFIX.
*倉庫マスタ
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
     JOINING  SOK       AS        PREFIX.
*名称マスタ
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
     JOINING  MEI       AS        PREFIX.
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
 01  LINE-CNT                PIC  9(02)     VALUE  99.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  MEIS-CNT                PIC  9(07)     VALUE  ZERO.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  ZSOKMS1-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
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
     03  RCV-STATUS        PIC  X(02).
     03  TAN-STATUS        PIC  X(02).
     03  SOK-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  PRT-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0440L".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0440L".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0440L".
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
*01  HD00.
*    03  FILLER              PIC  X(116)    VALUE  SPACE.
*    03  FILLER              PIC  X(19)     VALUE
*        "+-----+-----+-----+".
 01  HD01.
     03  FILLER         CHARACTER  TYPE YA-22.
         05  FILLER          PIC  X(01)    VALUE SPACE.
         05  FILLER          PIC  X(08)    VALUE "NKE0440L".
         05  FILLER          PIC  X(33)    VALUE SPACE.
         05  FILLER          PIC  N(11)    VALUE
         NC"＜　棚卸実績リスト　＞".
     03  FILLER         CHARACTER  TYPE YA.
         05  FILLER          PIC  X(18)    VALUE SPACE.
         05  FILLER          PIC  N(05)    VALUE NC"取込日付：".
         05  HD01-YYYY       PIC  9(04).
         05  FILLER          PIC  N(01)    VALUE NC"年".
         05  HD01-MM         PIC  Z9.
         05  FILLER          PIC  N(01)    VALUE NC"月".
         05  HD01-DD         PIC  Z9.
         05  FILLER          PIC  N(01)    VALUE NC"日".
         05  FILLER          PIC  X(02)    VALUE SPACE.
         05  HD00-PCNT       PIC  ZZ9.
         05  FILLER          PIC  N(01)    VALUE NC"頁".
 01  HD02.
     03  FILLER        CHARACTER  TYPE YA.
         05  FILLER          PIC  X(104)   VALUE SPACE.
         05  FILLER          PIC  N(05)    VALUE NC"取込時刻：".
         05  FILLER          PIC  X(02)    VALUE SPACE.
         05  HD02-JTIMEH     PIC  99.
         05  FILLER          PIC  N(01)    VALUE NC"：".
         05  HD02-JTIMEM     PIC  Z9.
         05  FILLER          PIC  N(01)    VALUE NC"：".
         05  HD02-JTIMED     PIC  Z9.
 01  HD03.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)    VALUE SPACE.
         05  FILLER          PIC  N(06)    VALUE
             NC"棚卸倉庫　：".
         05  HD03-SOKCD      PIC  X(02).
         05  FILLER          PIC  X(06)    VALUE SPACE.
         05  HD03-SOKNM      PIC  N(10).
*
 01  HD04.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  FILLER          PIC  N(06)     VALUE
             NC"取込担当者：".
         05  HD04-TANCD      PIC  X(07).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  HD04-TANNM      PIC  N(10).
 01  HD05.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(01)    VALUE  SPACE.
         05  FILLER          PIC  N(03)    VALUE
             NC"棚卸_".
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(02)    VALUE
             NC"棚番".
         05  FILLER          PIC  X(05)    VALUE  SPACE.
         05  FILLER          PIC  N(04)    VALUE
             NC"商品ＣＤ".
         05  FILLER          PIC  X(01)    VALUE  SPACE.
         05  FILLER          PIC  N(04)    VALUE
             NC"品単ＣＤ".
         05  FILLER          PIC  X(05)    VALUE  SPACE.
         05  FILLER          PIC  N(03)    VALUE
             NC"商品名".
         05  FILLER          PIC  X(19)    VALUE  SPACE.
         05  FILLER          PIC  N(05)    VALUE
             NC"ＪＡＮＣＤ".
         05  FILLER          PIC  X(06)    VALUE  SPACE.
         05  FILLER          PIC  N(04)    VALUE
             NC"棚卸数量".
         05  FILLER          PIC  X(05)    VALUE  SPACE.
         05  FILLER          PIC  N(04)    VALUE
             NC"帳簿在庫".
         05  FILLER          PIC  X(11)    VALUE  SPACE.
         05  FILLER          PIC  N(02)    VALUE
             NC"差数".
         05  FILLER          PIC  X(03)    VALUE  SPACE.
         05  FILLER          PIC  N(02)    VALUE
             NC"部門".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"実".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"立".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"承".
*
 01  HD06.
     03  FILLER         CHARACTER TYPE YA.
         05  FILLER          PIC  X(127)   VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"施".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"会".
         05  FILLER          PIC  X(02)    VALUE  SPACE.
         05  FILLER          PIC  N(01)    VALUE
             NC"認".
*
 01  SEN.
     03  FILLER              PIC  X(136)    VALUE  ALL "-".
*
*
 01  MS01.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANANO     PIC  9(07).
         05  FILLER          PIC  X(02)     VALUE  SPACE.
         05  MS01-TANABAN    PIC  X(06).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SHOCD      PIC  X(23).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-SHONM1     PIC  N(15).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-JANCD      PIC  X(13).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-TANASU     PIC  Z,ZZZ,ZZ9.99.
         05  MS01-TANASUR    REDEFINES      MS01-TANASU.
             07  MS01-TANASUX    PIC  X(12).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-CYOUBO     PIC  Z,ZZZ,ZZ9.99.
         05  MS01-CYOUBOR    REDEFINES      MS01-CYOUBO.
             07  MS01-CYOUBOX    PIC  X(12).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-KAKKOF     PIC  X(01).
*↓2019/01/07
*        05  MS01-SASU       PIC  Z,ZZZ,ZZ9.99.
*        05  MS01-SASUR      REDEFINES      MS01-SASU.
*            07  MS01-SASUX      PIC  X(12).
         05  MS01-SASU       PIC  --,---,--9.99.
         05  MS01-SASUR      REDEFINES      MS01-SASU.
             07  MS01-SASUX      PIC  X(13).
*↑2019/01/07
         05  MS01-KAKKOT     PIC  X(02).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
         05  MS01-BUMON      PIC  X(04).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
*↓2019/02/01
*        05  MS01-JISSI      PIC  X(02).
         05  MS01-JISSI      PIC  X(03).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  MS01-TACHIAI    PIC  X(02).
         05  MS01-TACHIAI    PIC  X(03).
         05  FILLER          PIC  X(01)     VALUE  SPACE.
*        05  MS01-SYOUNIN    PIC  X(02).
         05  MS01-SYOUNIN    PIC  X(03).
*↑2019/02/01
 01  MS02.
     03  FILLER         CHARACTER TYPE YB.
         05  FILLER          PIC  X(41)     VALUE  SPACE.
         05  MS01-SHONM2     PIC  N(15).
*
 01  P-SPACE            PIC  X(01)     VALUE  SPACE.
 01  P-LINE1            PIC  X(136)    VALUE  ALL   "-".
 01  P-LINE2            PIC  X(136)    VALUE  ALL   "=".
*編集
*01  WK-MS01-MNOUDT     PIC  X(10)     VALUE  SPACE.
*
*ブレイクキー
*01  WK-B-KEY.
*    03  WK-B-TOKCD     PIC  9(08)     VALUE  ZERO.
*    03  WK-B-DENKU     PIC  X(02)     VALUE  SPACE.
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
 LINKAGE                   SECTION.
 01  PARA-IN-BUMON         PIC X(04).
 01  PARA-IN-TANTOU        PIC X(02).
 01  PARA-IN-SOUKO         PIC X(02).
 01  PARA-IN-TDATE         PIC 9(08).
 01  PARA-IN-TTIME         PIC 9(06).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  PARA-IN-BUMON
                                         PARA-IN-TANTOU
                                         PARA-IN-SOUKO
                                         PARA-IN-TDATE
                                         PARA-IN-TTIME.
*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCVTANXX.
     MOVE      "RCVTANXX"   TO   AB-FILE.
     MOVE      RCV-STATUS   TO   AB-STS.
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
                        PROCEDURE   ZSOKMS1.
     MOVE      "ZSOKMS1 "   TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
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
     OPEN     INPUT     RCVTANXX  TANMS1  ZSOKMS1  MEIMS1
*
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
*棚卸確定データ読込
     PERFORM RCVTANXX-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃出力対象データ無！＃＃"  UPON CONS
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    棚卸確定データ読込
****************************************************************
 RCVTANXX-READ-SEC           SECTION.
*
     MOVE    "RCVTANXX-READ-SEC"   TO   S-NAME.
*
     READ     RCVTANXX  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   RCVTANXX-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   READ-CNT.
*
 RCVTANXX-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*取引先ＣＤ／伝票区分ブレイクチェック
*    IF       RCV-F01  NOT =  WK-B-TOKCD
*    OR       RCV-F02  NOT =  WK-B-DENKU
*             IF   IX  =  1
*                  PERFORM  MEISAI-WT-SEC
*             END-IF
*             PERFORM  HEAD-WT-SEC
*             MOVE RCV-F01       TO   WK-B-TOKCD
*             MOVE RCV-F02       TO   WK-B-DENKU
*             MOVE ZERO          TO   IX
*             MOVE SPACE         TO   MS01
*    END-IF.
*明細行初期化
     MOVE     SPACE              TO   MS01.
*    IF  IX  =  ZERO
*        MOVE   SPACE            TO   MS01
*    END-IF.
*
*    ADD     1                   TO    IX.
*****棚卸_
     MOVE    RCV-F01             TO    MS01-TANANO.
*****棚番
     MOVE    RCV-F04             TO    MS01-TANABAN.
*****商品ＣＤ　品単ＣＤ
     MOVE    "( "                TO    MS01-SHOCD(1:2).
     MOVE    RCV-F06(1:8)        TO    MS01-SHOCD(3:8).
     MOVE    " "                 TO    MS01-SHOCD(11:1).
     MOVE    RCV-F06(9:5)        TO    MS01-SHOCD(12:5).
     MOVE    "-"                 TO    MS01-SHOCD(17:1).
     MOVE    RCV-F06(14:2)       TO    MS01-SHOCD(18:2).
     MOVE    "-"                 TO    MS01-SHOCD(20:1).
     MOVE    RCV-F06(16:1)       TO    MS01-SHOCD(21:1).
     MOVE    " )"                TO    MS01-SHOCD(22:2).
*****商品名称取得
     MOVE    RCV-F06(1:8)        TO    MEI-F011.
     MOVE    RCV-F06(9:5)        TO    MEI-F0121.
     MOVE    RCV-F06(14:2)       TO    MEI-F0122.
     MOVE    RCV-F06(16:1)       TO    MEI-F0123.
     PERFORM  MEIMS1-READ-SEC.
     IF   MEIMS1-INV-FLG  =  "INV"
          MOVE   ALL NC"？"      TO    MS01-SHONM1
          MOVE   ALL NC"　"      TO    MS01-SHONM2
     ELSE
          MOVE   MEI-F021        TO    MS01-SHONM1
          MOVE   MEI-F022        TO    MS01-SHONM2
     END-IF.
*****ＪＡＮＣＤ
     MOVE    RCV-F05             TO    MS01-JANCD.
*****棚卸数量
     COMPUTE MS01-TANASU   =     RCV-F08  /  100.
*****帳簿在庫数
     COMPUTE MS01-CYOUBO   =     RCV-F07  /  100.
*****差数
     MOVE    "("                 TO    MS01-KAKKOF.
*↓2019/01/07
*    COMPUTE MS01-SASU     =     RCV-F09  /  100.
     COMPUTE MS01-SASU     =     ( RCV-F08 / 100 )
                               - ( RCV-F07 / 100 ).
*↑2019/01/07
     MOVE    " )"                TO    MS01-KAKKOT.
*****部門
     MOVE    RCV-F02             TO    MS01-BUMON.
*↓2019/02/01
*****実施担当者
*    MOVE    RCV-F10             TO    MS01-JISSI.
     MOVE    RCV-F17             TO    MS01-JISSI.
*****立会担当者
*    MOVE    RCV-F11             TO    MS01-TACHIAI.
     MOVE    RCV-F18             TO    MS01-TACHIAI.
*****承認担当者
*    MOVE    RCV-F12             TO    MS01-SYOUNIN.
     MOVE    RCV-F19             TO    MS01-SYOUNIN.
*↑2019/02/01
*****明細出力
     PERFORM  MEISAI-WT-SEC.
*    IF   IX  =  2
*         PERFORM  MEISAI-WT-SEC
*    END-IF.
*棚卸確定データ読込
     PERFORM RCVTANXX-READ-SEC.
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
*    IF       PAGE-CNT  >   ZERO
*             MOVE      SPACE     TO   PRT-REC
*             WRITE     PRT-REC   AFTER   PAGE
*    END-IF.
*    行カウンター初期化
*    MOVE     ZERO                TO   LINE-CNT.
*
*    改頁判定 行カウンター初期化
     IF       PAGE-CNT  >   ZERO
              MOVE      SPACE     TO   PRT-REC
              WRITE     PRT-REC   AFTER   PAGE
              MOVE      1         TO   LINE-CNT
     ELSE
              MOVE      ZERO      TO   LINE-CNT
     END-IF.
*    頁カウンター
     ADD      1                   TO   PAGE-CNT.
     MOVE     PAGE-CNT            TO   HD00-PCNT.
*    システム日付セット
*    MOVE     SYS-DATEW(1:4)      TO   HD01-YYYY.
*    MOVE     SYS-DATEW(5:2)      TO   HD01-MM.
*    MOVE     SYS-DATEW(7:2)      TO   HD01-DD.
*    取込日付セット
     MOVE     PARA-IN-TDATE(1:4)  TO   HD01-YYYY.
     MOVE     PARA-IN-TDATE(5:2)  TO   HD01-MM.
     MOVE     PARA-IN-TDATE(7:2)  TO   HD01-DD.
*    システム時刻セット
*    MOVE     WK-TIME-HM(1:2)     TO   HD02-JTIMEH.
*    MOVE     ":"                 TO   HD02-JTIME(3:1).
*    MOVE     WK-TIME-HM(3:2)     TO   HD02-JTIMEM.
*    MOVE     ":"                 TO   HD02-JTIME(6:1).
*    MOVE     WK-TIME-HM(5:2)     TO   HD02-JTIMED.
*    取込時刻セット
     MOVE     PARA-IN-TTIME(1:2)  TO   HD02-JTIMEH.
     MOVE     PARA-IN-TTIME(3:2)  TO   HD02-JTIMEM.
     MOVE     PARA-IN-TTIME(5:2)  TO   HD02-JTIMED.
*    倉庫名取得
     MOVE     PARA-IN-SOUKO       TO   SOK-F01 HD03-SOKCD.
     PERFORM  ZSOKMS1-READ-SEC.
     IF       ZSOKMS1-INV-FLG = SPACE
              MOVE SOK-F02        TO   HD03-SOKNM
     ELSE
              MOVE ALL NC"？"     TO   HD03-SOKNM
     END-IF.
*    担当者名取得
     MOVE     PARA-IN-BUMON       TO   TAN-F01 HD04-TANCD(1:4).
     MOVE     PARA-IN-TANTOU      TO   TAN-F02 HD04-TANCD(6:2).
     MOVE     "-"                 TO   HD04-TANCD(5:1).
     PERFORM  TANMS1-READ-SEC.
     IF       TANMS1-INV-FLG = SPACE
              MOVE TAN-F03        TO   HD04-TANNM
     ELSE
              MOVE ALL NC"？"     TO   HD04-TANNM
     END-IF.
*    変換日付
*    MOVE     RCV-F96(1:4)     TO   HD04-HENDATE(1:4).
*    MOVE     "/"              TO   HD04-HENDATE(5:1).
*    MOVE     RCV-F96(5:2)     TO   HD04-HENDATE(6:2).
*    MOVE     "/"              TO   HD04-HENDATE(8:1).
*    MOVE     RCV-F96(7:2)     TO   HD04-HENDATE(9:2).
*    システム時刻セット
*    MOVE     RCV-F97(1:2)     TO   HD04-HENTIME(1:2).
*    MOVE     ":"              TO   HD04-HENTIME(3:1).
*    MOVE     RCV-F97(3:2)     TO   HD04-HENTIME(4:2).
*    MOVE     ":"              TO   HD04-HENTIME(6:1).
*    MOVE     RCV-F97(5:2)     TO   HD04-HENTIME(7:2).
*    伝票区分取得
*    MOVE     01                  TO   JYO-F01.
*    MOVE     RCV-F02             TO   JYO-F02 HD05-DENKU.
*    PERFORM  HJYOKEN-READ-SEC.
*    IF  HJYOKEN-INV-FLG = SPACE
*              MOVE JYO-F03    TO   HD05-DENNM
*    ELSE
*              MOVE ALL NC"？" TO   HD05-DENNM
*    END-IF.
*    取引先名取得
*    MOVE     RCV-F01             TO   SOK-F01 HD05-TOKCD.
*    PERFORM  ZSOKMS1-READ-SEC.
*    IF  ZSOKMS1-INV-FLG = SPACE
*              MOVE SOK-F02    TO   HD05-TOKNM
*    ELSE
*              MOVE ALL NC"？" TO   HD05-TOKNM
*    END-IF.
*    ヘッダ印刷
     WRITE    PRT-REC       FROM  HD01  AFTER  2.
     WRITE    PRT-REC       FROM  HD02  AFTER  1.
     WRITE    PRT-REC       FROM  HD03  AFTER  1.
     WRITE    PRT-REC       FROM  HD04  AFTER  1.
     WRITE    PRT-REC       FROM  SEN   AFTER  2.
     WRITE    PRT-REC       FROM  HD05  AFTER  2.
     WRITE    PRT-REC       FROM  HD06  AFTER  1.
     MOVE     SPACE         TO    PRT-REC.
     WRITE    PRT-REC                   AFTER  1.
*行カウントアップ
     MOVE     11                  TO    LINE-CNT.
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
     IF       LINE-CNT  >   50
              PERFORM HEAD-WT-SEC
     END-IF.
*    明細印刷
     WRITE    PRT-REC       FROM  MS01  AFTER  1.
     WRITE    PRT-REC       FROM  MS02  AFTER  1.
*行カウント
     ADD      2                   TO    LINE-CNT.
*明細カウント
     ADD      1                   TO    MEIS-CNT.
*添字クリア
*    MOVE     ZERO                TO    IX.
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
     CLOSE     RCVTANXX TANMS1  ZSOKMS1  MEIMS1   PRTF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    担当者マスタ索引
****************************************************************
 TANMS1-READ-SEC            SECTION.
*
     MOVE     "TANMS1-READ-SEC"  TO  S-NAME.
*
     READ     TANMS1
       INVALID
              MOVE "INV"     TO   TANMS1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
     EXIT.
****************************************************************
*    倉庫マスタ索引
****************************************************************
 ZSOKMS1-READ-SEC            SECTION.
*
     MOVE     "ZSOKMS1-READ-SEC"  TO  S-NAME.
*
     READ     ZSOKMS1
       INVALID
              MOVE "INV"     TO   ZSOKMS1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   ZSOKMS1-INV-FLG
     END-READ.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
****************************************************************
*    名称マスタ索引
****************************************************************
 MEIMS1-READ-SEC            SECTION.
*
     MOVE     "MEIMS1-READ-SEC"  TO  S-NAME.
*
     READ     MEIMS1
       INVALID
              MOVE "INV"     TO   MEIMS1-INV-FLG
       NOT  INVALID
              MOVE SPACE     TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
