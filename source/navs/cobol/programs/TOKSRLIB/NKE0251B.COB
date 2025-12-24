****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　入荷検品　　　　　　　            *
*    モジュール名　　　　：　入荷検品データ取込（不良品）　　　*
*    処理概要　　　　　　：　入荷確定累積ファイルより、　　　　*
*                            不良品ＤＴを不良品累積Ｆに出力する*
*    流用　　　　　　　　：　NKE0251B                          *
*    作成日／作成者　　　：　2019/02/01 T.TAKAHASHI            *
*    更新日／更新者　　　：　                                  *
*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NKE0251B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/02/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     STATION   IS   STAT
     CONSOLE   IS   CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*入荷確定累積ファイル
     SELECT   RUINYKL3  ASSIGN    TO        DA-01-VI-RUINYKL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RUI-F01
                                            RUI-F96
                                            RUI-F97
                                            RUI-F04
                                            RUI-F02
                                            RUI-F03
                        FILE  STATUS   IS   RUI-STATUS.
*商品名称マスタ
     SELECT     MEIMS1  ASSIGN    TO       DA-01-VI-MEIMS1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      MEI-F011  MEI-F0121
                                           MEI-F0122 MEI-F0123
                        FILE      STATUS   MEI-STATUS.
*倉庫不良品累積ファイル
     SELECT     SKFRRKF ASSIGN    TO       DA-01-VI-SKFRRKL1
                        ORGANIZATION       INDEXED
                        ACCESS    MODE     RANDOM
                        RECORD    KEY      FRR-F01  FRR-F02
                                           FRR-F03  FRR-F08
                                           FRR-F05
                        FILE      STATUS   FRR-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*入荷確定累積ファイル
 FD  RUINYKL3           LABEL RECORD   IS   STANDARD.
     COPY     RUINYKL3   OF        XFDLIB
     JOINING  RUI       PREFIX.
*商品名称マスタ
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     MEIMS1    OF        XFDLIB
     JOINING  MEI       AS        PREFIX.
*倉庫不良品累積ファイル
 FD  SKFRRKF            LABEL RECORD   IS   STANDARD.
     COPY     SKFRRKF   OF        XFDLIB
     JOINING  FRR       AS        PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  RUINYKL3-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  SKFRRKF-WT-CNT          PIC  9(07)     VALUE  ZERO.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-ERR-CNT          PIC  9(07)     VALUE  ZERO.
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
     03  RUI-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  FRR-STATUS        PIC  X(02).
*メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NKE0251B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0251B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NKE0251B".
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
 01  PARA-IN-SOKCD         PIC   X(02).
 01  PARA-IN-TORIDT        PIC   9(08).
 01  PARA-IN-TORITM        PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION
                                 USING PARA-IN-BUMON
                                       PARA-IN-TANCD
                                       PARA-IN-SOKCD
                                       PARA-IN-TORIDT
                                       PARA-IN-TORITM.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RUINYKL3.
     MOVE      "RUINYKL3"    TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
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
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SKFRRKF.
     MOVE      "SKFRRKL1"   TO   AB-FILE.
     MOVE      FRR-STATUS   TO   AB-STS.
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
     OPEN     INPUT     RUINYKL3
                        MEIMS1
     OPEN     I-O       SKFRRKF.
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
*入荷確定累積ファイルスタート
     PERFORM  RUINYKL3-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          GO                    TO    INIT-EXIT
     END-IF.
*入荷確定累積ファイル読込
     PERFORM RUINYKL3-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　対象データ　　なし　＃＃"  UPON CONS
          GO                    TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    入荷確定累積ファイルスタート
****************************************************************
 RUINYKL3-START-SEC          SECTION.
*
     MOVE    "RUINYKL3-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   RUI-REC.
     INITIALIZE                        RUI-REC.
*
     MOVE     PARA-IN-SOKCD       TO   RUI-F01.
     MOVE     PARA-IN-TORIDT      TO   RUI-F96.
     MOVE     PARA-IN-TORITM      TO   RUI-F97.
     MOVE     ZERO                TO   RUI-F04.
     MOVE     ZERO                TO   RUI-F02.
     MOVE     ZERO                TO   RUI-F03.
*
     START    RUINYKL3  KEY  IS  >=    RUI-F01 RUI-F96
                                       RUI-F97 RUI-F04
                                       RUI-F02 RUI-F03
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 RUINYKL3-START-EXIT.
     EXIT.
*
****************************************************************
*    入荷確定累積ファイル読込
****************************************************************
 RUINYKL3-READ-SEC           SECTION.
*
     MOVE    "RUINYKL3-READ-SEC"   TO   S-NAME.
*
 RUINYKL3-READ-01.
     READ     RUINYKL3  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   RUINYKL3-READ-EXIT
     END-READ.
*
     IF     ( RUI-F01 = PARA-IN-SOKCD  ) AND
            ( RUI-F96 = PARA-IN-TORIDT ) AND
            ( RUI-F97 = PARA-IN-TORITM )
              CONTINUE
     ELSE
              MOVE     "END"      TO   END-FLG
              GO                  TO   RUINYKL3-READ-EXIT
     END-IF.
*
*エラー分は対象外
     IF       RUI-F22  =  "1"
              GO     TO    RUINYKL3-READ-01
     END-IF.
*不良品数チェック
     IF       RUI-F15  <=  ZERO
              GO     TO    RUINYKL3-READ-01
     END-IF.
*件数カウント
     ADD      1                   TO   RUINYKL3-READ-CNT.
*
 RUINYKL3-READ-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*初期化
     MOVE     SPACE              TO   FRR-REC.
     INITIALIZE                       FRR-REC.
*項目セット
     MOVE     PARA-IN-SOKCD      TO   FRR-F01.
     MOVE     RUI-F96            TO   FRR-F02.
     MOVE     RUI-F97            TO   FRR-F03.
     MOVE     RUI-F17            TO   FRR-F04.
     MOVE     RUI-F05            TO   FRR-F05.
     MOVE     RUI-F06            TO   FRR-F06.
     MOVE     RUI-F07            TO   FRR-F07.
     MOVE     RUI-F06            TO   MEI-F011.
     MOVE     RUI-F07            TO   MEI-F012.
     PERFORM  MEIMS1-READ-SEC.
     IF   MEIMS1-INV-FLG  =  "INV"
          DISPLAY NC"＃商品名称Ｍ未登録＃" RUI-F06 "-" RUI-F07
                  UPON CONS
          ADD     1              TO   MEIMS1-ERR-CNT
          GO                     TO   MAIN-010
     ELSE
          MOVE  MEI-F05          TO   FRR-F08
     END-IF.
     MOVE     RUI-F15            TO   FRR-F09.
     MOVE     RUI-F16            TO   FRR-F10.
     MOVE     RUI-F02            TO   FRR-F11.
     MOVE     RUI-F03            TO   FRR-F12.
     MOVE     SYS-DATEW          TO   FRR-F96.
     MOVE     WK-TIME-HM         TO   FRR-F97.
     MOVE     PARA-IN-BUMON      TO   FRR-F98.
     MOVE     PARA-IN-TANCD      TO   FRR-F99.
*
     WRITE    FRR-REC.
     ADD      1                  TO   SKFRRKF-WT-CNT.
*
 MAIN-010.
*入荷確定累積ファイル読込
     PERFORM RUINYKL3-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数表示
*  ＝入荷確定累積ファイル読込件数
     DISPLAY "RUINYKL3 READ-CNT  = " RUINYKL3-READ-CNT UPON CONS.
     DISPLAY "SKFRRKF-WT-CNT     = " SKFRRKF-WT-CNT    UPON CONS.
     DISPLAY "MEIMS1-ERR-CNT     = " MEIMS1-ERR-CNT    UPON CONS.
*
     CLOSE     RUINYKL3  MEIMS1  SKFRRKF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*    商品名称マスタ索引
****************************************************************
 MEIMS1-READ-SEC           SECTION.
     MOVE "MEIMS1-READ-SEC"       TO   S-NAME.
*
     READ       MEIMS1
         INVALID       MOVE "INV" TO   MEIMS1-INV-FLG
         NOT  INVALID  MOVE SPACE TO   MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
