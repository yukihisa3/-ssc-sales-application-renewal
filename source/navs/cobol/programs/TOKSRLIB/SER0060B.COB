****************************************************************
*    顧客名　　　　　：　_サカタのタネ　営業第２部向け　　　　*
*    業務名　　　　　：　システム共通サブシステム　　　　　　　*
*    モジュール名　　：　受注売上リカバリーキーデータ作成      *
*    作成日／作成者　：　2021/12/06 INOUE                      *
*    処理概要　　　　：　指定管理番号と合致する　　　　　　　　*
*    　　　　　　　　　　リカバリ対象ＫＥＹを抽出する　　　　　*
*    更新日／更新者　：　                                      *
****************************************************************
 IDENTIFICATION         DIVISION.
 PROGRAM-ID.            SER0060B.
*                  流用:SER0030B.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/12/06.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       PG6000.
 OBJECT-COMPUTER.       PG6000.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*Ｄ３６５累積ファイル
     SELECT  ERRRUIL1   ASSIGN    TO   DA-01-VI-ERRRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS SEQUENTIAL
                        RECORD    KEY    IS ERR-F01
                        FILE      STATUS IS ERR-STATUS.
*Ｄ３６５エラー状況ファイル
     SELECT   ERRJYOL1  ASSIGN    TO   DA-01-VI-ERRJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS RANDOM
                        RECORD    KEY    IS ERJ-F01
                        FILE      STATUS IS ERJ-STATUS.
*受注売上調整計上データ
     SELECT   URIKEJL5  ASSIGN    TO   DA-01-VI-URIKEJL5
                        ORGANIZATION        INDEXED
                        ACCESS    MODE   IS RANDOM
                        RECORD    KEY    IS KEJ-F09
                        FILE      STATUS IS KEJ-STATUS.
*受注売上キャンセルファイル
     SELECT   CANURIK1  ASSIGN    TO        DA-01-VI-CANURIK1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       CAN-F01
                                            CAN-F02
                                            CAN-F03
                                            CAN-F04
                                            CAN-F05
                                            CAN-F06
                        FILE  STATUS   IS   CAN-STATUS.
***************************************************************
*INPUT-OUTPUT           SECTION.
******************************************************************
 DATA                      DIVISION.
*--------------------------------------------------------------*
 FILE                   SECTION.
*--------------------------------------------------------------*
*FD    ERR365F     LABEL      RECORD    IS    STANDARD.
*      COPY        ERR365F    OF    XFDLIB
*                  JOINING    ERT   PREFIX.
 FD    ERRRUIL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRRUIL1   OF    XFDLIB
                   JOINING    ERR   PREFIX.
 FD    ERRJYOL1    LABEL      RECORD    IS    STANDARD.
       COPY        ERRJYOL1   OF    XFDLIB
                   JOINING    ERJ   PREFIX.
 FD    URIKEJL5    LABEL      RECORD    IS    STANDARD.
       COPY        URIKEJL5   OF    XFDLIB
                   JOINING    KEJ   PREFIX.
 FD    CANURIK1    LABEL      RECORD    IS    STANDARD.
       COPY        CANURIK1   OF    XFDLIB
                   JOINING    CAN   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-RENBAN               PIC  9(08).
 01  ERR-STATUS              PIC  X(02).
 01  ERJ-STATUS              PIC  X(02).
 01  KEJ-STATUS              PIC  X(02).
 01  CAN-STATUS              PIC  X(02).
 01  ERR-END-FLG             PIC  X(01).
 01  ERJ-INV-FLG             PIC  X(01).
 01  KEJ-INV-FLG             PIC  X(01).
 01  CAN-INV-FLG             PIC  X(01).
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
*件数
 01  RD-CNT                  PIC  9(07).
 01  WT-CNT                  PIC  9(07).
 01  ER01-CNT                PIC  9(07).
 01  ER02-CNT                PIC  9(07).
 01  ER03-CNT                PIC  9(07).
 01  ER04-CNT                PIC  9(07).
 01  ER05-CNT                PIC  9(07).
*日付
 01  SYS-DATE                PIC  9(06)  VALUE  ZERO.
 01  SYS-DATEW               PIC  9(08)  VALUE  ZERO.
*時刻
 01  TIME-AREA.
     03  WK-TIME             PIC  9(08)  VALUE  ZERO.
*
*ＭＳＧエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  ST-PG           PIC  X(08) VALUE "SER0060B".
         05  FILLER          PIC  X(11)
                                  VALUE " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0060B".
         05  FILLER          PIC  X(11)
                                  VALUE "  END  *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  END-PG          PIC  X(08) VALUE "SER0060B".
         05  FILLER          PIC  X(11)
                                  VALUE " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06) VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05) VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05) VALUE " *** ".
         05  FILLER          PIC  X(07) VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
*
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC  X(01).
     03  LINK-IN-YMD6        PIC  9(06).
     03  LINK-IN-YMD8        PIC  9(08).
     03  LINK-OUT-RET        PIC  X(01).
     03  LINK-OUT-YMD8       PIC  9(08).
*
*--------------------------------------------------------------*
 LINKAGE                SECTION.
*--------------------------------------------------------------*
 01  LINK-IN-BUMCD           PIC  X(04).
 01  LINK-IN-TANCD           PIC  X(02).
 01  LINK-IN-KANRINO         PIC  9(08).
 01  LINK-IN-BDATE           PIC  9(08).
 01  LINK-IN-BTIME           PIC  9(06).
*
******************************************************************
 PROCEDURE    DIVISION       USING
                                       LINK-IN-BUMCD
                                       LINK-IN-TANCD
                                       LINK-IN-KANRINO
                                       LINK-IN-BDATE
                                       LINK-IN-BTIME.
******************************************************************
 DECLARATIVES.
 FILEERR-SEC2              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     ERRRUIL1.
     MOVE     "ERRRUIL1"   TO     AB-FILE.
     MOVE     ERR-STATUS   TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC3              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     ERRJYOL1.
     MOVE     "ERRJYOL1"   TO     AB-FILE.
     MOVE     ERJ-STATUS   TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC4              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     URIKEJL5.
     MOVE     "URIKEJL5"   TO     AB-FILE.
     MOVE     KEJ-STATUS   TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 FILEERR-SEC5              SECTION.
     USE      AFTER        EXCEPTION
                           PROCEDURE     CANURIK1.
     MOVE     "CANURIK1"   TO     AB-FILE.
     MOVE     CAN-STATUS   TO     AB-STS.
     DISPLAY  MSG-ABEND           UPON CONS.
     DISPLAY  SEC-NAME            UPON CONS.
     DISPLAY  ABEND-FILE          UPON CONS.
     MOVE     4000         TO     PROGRAM-STATUS.
     STOP     RUN.
 END      DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     MOVE     "PROG-CNTL" TO   SEC-NAME.
*
 000-PROG-01.
     OPEN     INPUT     URIKEJL5.
     OPEN     I-O       ERRRUIL1  ERRJYOL1  CANURIK1.
     MOVE     ZERO      TO        ER01-CNT
                                  ER02-CNT
                                  ER03-CNT
                                  ER04-CNT
                                  ER05-CNT
                                  RD-CNT
                                  WT-CNT.
*
*システム日付編集*
 000-PROG-02.
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
*システム時間取得
     ACCEPT    WK-TIME          FROM TIME.
*
 000-PROG-03.
*エラー状況ファイル検索
     PERFORM  ERJ-READ.
     IF       ERJ-INV-FLG NOT =  SPACE
              DISPLAY "# エラー状況ファイルなし #" UPON CONS
              MOVE   4001    TO     PROGRAM-STATUS
              GO             TO     000-PROG-08
     END-IF.
*
 000-PROG-04.
*累積ファイルＳＴＡＲＴ
     MOVE  SPACE                TO   ERR-REC.
     INITIALIZE                      ERR-REC.
     MOVE   LINK-IN-KANRINO     TO   ERR-F01.
     START  ERRRUIL1  KEY  >=   ERR-F01
            INVALID   KEY
               DISPLAY NC"＃対象データ無し１＃" UPON CONS
               MOVE    4001        TO   PROGRAM-STATUS
               GO                  TO   000-PROG-08
     END-START.
*
*累積ファイル読込
 000-PROG-05.
     PERFORM  ERR-READ.
     IF       ERR-END-FLG NOT =  SPACE
              DISPLAY NC"＃対象データ無し２＃" UPON CONS
              MOVE    4001      TO   PROGRAM-STATUS
              GO                TO   000-PROG-08
     END-IF.
*メイン
 000-PROG-06.
     PERFORM  MAIN-SEC  UNTIL  END-FLG = "END".
*
*エラー状況ファイル更新
     IF       ER01-CNT NOT = ZERO
              MOVE     ER01-CNT      TO     ERJ-F062
              MOVE     LINK-IN-BUMCD TO     ERJ-F063
              MOVE     LINK-IN-TANCD TO     ERJ-F064
              MOVE     SYS-DATEW     TO     ERJ-F065
              MOVE     WK-TIME(1:6)  TO     ERJ-F066
     END-IF.
     IF       ER02-CNT NOT = ZERO
              MOVE     ER02-CNT      TO     ERJ-F072
              MOVE     LINK-IN-BUMCD TO     ERJ-F073
              MOVE     LINK-IN-TANCD TO     ERJ-F074
              MOVE     SYS-DATEW     TO     ERJ-F075
              MOVE     WK-TIME(1:6)  TO     ERJ-F076
     END-IF.
     REWRITE  ERJ-REC.
*
 000-PROG-07.
     DISPLAY "管理番号" " = " LINK-IN-KANRINO UPON CONS.
     DISPLAY "バッチ日" " = " LINK-IN-BDATE   UPON CONS.
     DISPLAY "バッチ時" " = " LINK-IN-BTIME   UPON CONS.
     DISPLAY "読込件数" " = " RD-CNT          UPON CONS.
     DISPLAY "伝票枚数" " = " WT-CNT          UPON CONS.
     DISPLAY "受注明細" " = " ER01-CNT        UPON CONS.
     DISPLAY "売上明細" " = " ER02-CNT        UPON CONS.
*表示
 000-PROG-08.
     CLOSE    ERRRUIL1 ERRJYOL1 URIKEJL5 CANURIK1.
*
 000-PROG-09.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   累積ファイル読込　　　                      *
*--------------------------------------------------------------*
 ERR-READ         SECTION.
*
     MOVE     "ERR-READ"     TO     SEC-NAME.
*
 ERR-READ-01.
     READ     ERRRUIL1
         AT END
              MOVE   "1"     TO     ERR-END-FLG
              GO             TO     ERR-READ-EXIT
         NOT AT END
              MOVE   SPACE   TO     ERR-END-FLG
              ADD    1       TO     RD-CNT
     END-READ.
*
     IF       ERR-F01  NOT = LINK-IN-KANRINO
              MOVE   "1"     TO     ERR-END-FLG
              GO             TO     ERR-READ-EXIT
     END-IF.
*
     IF     ( ERR-F043     = "01" )  OR
            ( ERR-F043     = "02" )
              CONTINUE
     ELSE
              GO             TO     ERR-READ-01
     END-IF.
*
 ERR-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   メイン処理                          *
*--------------------------------------------------------------*
 MAIN-SEC            SECTION.
     MOVE     "MAIN-SEC"       TO     SEC-NAME.
*
 MAIN-SEC-011.
*受注売上調整計上データ検索
     PERFORM  KEJ-READ.
     IF       KEJ-INV-FLG NOT =  SPACE
              DISPLAY "# 受注売上調整計上データなし #" UPON CONS
              DISPLAY "#  D365DEN= " ERR-F044(3:18)    UPON CONS
              DISPLAY "# スキップします　           #" UPON CONS
              GO             TO     MAIN-SEC-04
     END-IF.
*
 MAIN-SEC-012.
*エラー区分カウント
     EVALUATE ERR-F043
        WHEN  "01"
              ADD    1         TO     ER01-CNT
        WHEN  "02"
              ADD    1         TO     ER02-CNT
        WHEN  OTHER
              DISPLAY "# 想定外エラー区分 #"    UPON CONS
              DISPLAY "# スキップします　 #"    UPON CONS
              DISPLAY "  エラー区分= " ERR-F043 UPON CONS
              DISPLAY "  管理番号　= " LINK-IN-KANRINO UPON CONS
              GO               TO     MAIN-SEC-04
     END-EVALUATE.
*
 MAIN-SEC-02.
*受注売上キャンセルファイル検索
     PERFORM  CAN-READ.
     IF       CAN-INV-FLG NOT =  SPACE
              GO             TO     MAIN-SEC-031
     ELSE
              GO             TO     MAIN-SEC-032
     END-IF.
*
 MAIN-SEC-031.
*受注売上キャンセルファイル出力
     MOVE     SPACE            TO     CAN-REC.
     INITIALIZE                       CAN-REC.
     MOVE     KEJ-F01          TO     CAN-F01.
     MOVE     KEJ-F02          TO     CAN-F02.
     MOVE     KEJ-F03          TO     CAN-F03.
     MOVE     KEJ-F04          TO     CAN-F04.
     MOVE     KEJ-F05          TO     CAN-F05.
     MOVE     KEJ-F06          TO     CAN-F06.
     MOVE     1                TO     CAN-F07.
     WRITE    CAN-REC.
     ADD      1                TO     WT-CNT.
*
 MAIN-SEC-032.
*D365エラー累積ファイル更新
     MOVE     LINK-IN-TANCD    TO     ERR-F97.
     MOVE     SYS-DATEW        TO     ERR-F98.
     MOVE     WK-TIME(1:6)     TO     ERR-F99.
     REWRITE  ERR-REC.
*
 MAIN-SEC-04.
*D365エラー累積ファイル読込
     PERFORM  ERR-READ.
     IF       ERR-END-FLG  NOT =  SPACE
              MOVE   "END"      TO   END-FLG
              GO                TO   MAIN-SEC-EXIT
     END-IF.
*
  MAIN-SEC-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   エラー状況ファイル検索                      *
*--------------------------------------------------------------*
 ERJ-READ         SECTION.
*
     MOVE     "ERJ-READ"      TO     SEC-NAME.
*
     MOVE     LINK-IN-KANRINO TO     ERJ-F01.
*****DISPLAY "KEY = " ERJ-F01        UPON CONS.
     READ     ERRJYOL1
         INVALID
              MOVE   "1"      TO     ERJ-INV-FLG
         NOT INVALID
              MOVE   SPACE    TO     ERJ-INV-FLG
     END-READ.
*
 ERJ-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   受注売上調整計上データ検索                  *
*--------------------------------------------------------------*
 KEJ-READ         SECTION.
*
     MOVE     "KEJ-READ"      TO     SEC-NAME.
*
     MOVE     ERR-F044(3:18)  TO     KEJ-F09.
*****DISPLAY "KEY = " KEJ-F09        UPON CONS.
     READ     URIKEJL5
         INVALID
              MOVE   "1"      TO     KEJ-INV-FLG
         NOT INVALID
              MOVE   SPACE    TO     KEJ-INV-FLG
     END-READ.
*
 KEJ-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL   1.1   受注売上キャンセルファイル検索              *
*--------------------------------------------------------------*
 CAN-READ         SECTION.
*
     MOVE     "CAN-READ"      TO     SEC-NAME.
*
     MOVE     KEJ-F01         TO     CAN-F01.
     MOVE     KEJ-F02         TO     CAN-F02.
     MOVE     KEJ-F03         TO     CAN-F03.
     MOVE     KEJ-F04         TO     CAN-F04.
     MOVE     KEJ-F05         TO     CAN-F05.
     MOVE     KEJ-F06         TO     CAN-F06.
*****DISPLAY "KEY1= " CAN-F01        UPON CONS.
*****DISPLAY "KEY1= " CAN-F02        UPON CONS.
*****DISPLAY "KEY1= " CAN-F03        UPON CONS.
*****DISPLAY "KEY1= " CAN-F04        UPON CONS.
*****DISPLAY "KEY1= " CAN-F05        UPON CONS.
*****DISPLAY "KEY1= " CAN-F06        UPON CONS.
     READ     CANURIK1
         INVALID
              MOVE   "1"      TO     CAN-INV-FLG
         NOT INVALID
              MOVE   SPACE    TO     CAN-INV-FLG
     END-READ.
*
 CAN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
