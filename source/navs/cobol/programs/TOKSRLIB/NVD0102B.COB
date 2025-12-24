****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｄ３６５連携　　　　　　　　　　　*
*    モジュール名　　　　：　入荷予定データ取込＆重複ＣＨＫ　　*
*    作成日／作成者　　　：　2021/06/14   NAV TAKAHASHI        *
*    処理内容　　　　　　：　入荷予定取込データＦを読み、　　　*
*    　　　　　　　　　　　　入荷累積Ｆを読み、重複チェックを　*
*                            行なう。　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION              DIVISION.
 PROGRAM-ID.                 NVD0102B.
 ENVIRONMENT                 DIVISION.
 CONFIGURATION               SECTION.
 SOURCE-COMPUTER.
 OBJECT-COMPUTER.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*入荷予定データ
     SELECT  YTINYKF  ASSIGN        TO  DA-01-YTINYKF
                      ORGANIZATION  IS  SEQUENTIAL
                      ACCESS MODE   IS  SEQUENTIAL
                      FILE   STATUS IS  YTI-ST.
*入荷累積ファイル
     SELECT  NYKRUIF  ASSIGN        TO  NYKRUIL2
                      ORGANIZATION  IS  INDEXED
                      ACCESS MODE   IS  RANDOM
                      RECORD KEY    IS  NYK-F060
                                        NYK-F061
                      FILE   STATUS IS  NYK-ST.
*入荷予定チェックデータ
     SELECT  YTICHKF  ASSIGN        TO  DA-01-YTICHKF
                      ORGANIZATION  IS  SEQUENTIAL
                      ACCESS MODE   IS  SEQUENTIAL
                      FILE   STATUS IS  CHK-ST.
****************************************************************
 DATA                        DIVISION.
****************************************************************
 FILE                        SECTION.
*入荷予定データ
 FD  YTINYKF
     LABEL       RECORD      IS        STANDARD.
      COPY        YTINYKF     OF        XFDLIB
      JOINING     YTI         AS        PREFIX.
*入荷累積ファイル
 FD  NYKRUIF.
     COPY        NYKRUIF     OF        XFDLIB
     JOINING     NYK         AS        PREFIX.
*入荷予定\ＣＨＫデータ
 FD  YTICHKF
     LABEL       RECORD      IS        STANDARD.
      COPY        YTICHKF     OF        XFDLIB
      JOINING     CHK         AS        PREFIX.
****************************************************************
 WORKING-STORAGE           SECTION.
****************************************************************
 01  ST-AREA.
     03  YTI-ST              PIC  X(02)  VALUE  SPACE.
     03  NYK-ST              PIC  X(02)  VALUE  SPACE.
     03  CHK-ST              PIC  X(02)  VALUE  SPACE.

 01  END-FLG                 PIC  X(03)  VALUE  SPACE.

 01  CNT-AREA.
     03  YTI-CNT             PIC  9(07)  VALUE  ZERO.
     03  CHK-CNT1            PIC  9(07)  VALUE  ZERO.
     03  CHK-CNT2            PIC  9(07)  VALUE  ZERO.
     03  NG-CNT              PIC  9(07)  VALUE  ZERO.
 01  FLG-AREA.
     03  NYKRUIF-INV-FLG     PIC  X(03).

 01  WK-AREA.
     03  WK-DENNO            PIC  X(20).
     03  WK-NAVS-DENNO       PIC  X(10).
     03  WK-NAVS-DENNO-R          REDEFINES WK-NAVS-DENNO.
         05  WK-NAVS         PIC  9(10).
     03  WK-NAVS-MIN         PIC  9(10).
     03  WK-NAVS-MAX         PIC  9(10).

 01  CHK-AREA.
     03  CHK-ERR-KBN         PIC  X(01).
     03  CHK-ERR             PIC  X(01)  OCCURS 10.

*日付取得
 01  SYS-DATE                PIC  9(06).
 01  WK-DATE8.
     03  WK-Y                PIC  9(04).
     03  WK-M                PIC  9(02).
     03  WK-D                PIC  9(02).
 01  SYS-TIME.
     03  WK-TIME             PIC  9(06).
*
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
*
 01  FILE-ERR.
     03  YTI-ERR-NM         PIC  N(10)  VALUE
                   NC"入荷予定データ　異常".
     03  NYK-ERR-NM          PIC  N(10)  VALUE
                   NC"入荷累積データ　異常".
     03  CHK-ERR-NM          PIC  N(10)  VALUE
                   NC"入荷予定ＣＨＫ　異常".
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "NVD0102B".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "NVD0102B".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-OUT1.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 入荷予定データ　読込件数 = ".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT2.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(30)  VALUE
                             " 入荷予定ＣＨＫ　作成件数 = ".
         05  MSG-OUT02       PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC X(01).
 01  LINK-IN-YMD6            PIC 9(06).
 01  LINK-IN-YMD8            PIC 9(08).
 01  LINK-OUT-RET            PIC X(01).
 01  LINK-OUT-YMD            PIC 9(08).
****************************************************************
 PROCEDURE                   DIVISION.
****************************************************************
 DECLARATIVES.
 YTI-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE YTINYKF.
     DISPLAY     YTI-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     YTI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 NYK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE NYKRUIF.
     DISPLAY     NYK-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     NYK-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 CHK-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE YTICHKF.
     DISPLAY     CHK-ERR-NM  UPON      CONS.
     DISPLAY     SEC-NAME    UPON      CONS.
     DISPLAY     YTI-ST      UPON      CONS.
     MOVE        4000        TO        PROGRAM-STATUS.
     STOP        RUN.
 END DECLARATIVES.
****************************************************************
*                 P R O G R A M - S E C
****************************************************************
 PROGRAM-SEC                 SECTION.
     PERFORM     INIT-SEC.
     PERFORM     MAIN-SEC    UNTIL     END-FLG  =  "END".
     PERFORM     END-SEC.
     STOP        RUN.
*PROGRAM-END.
****************************************************************
*    初期処理
****************************************************************
 INIT-SEC                    SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.

*
     DISPLAY     MSG-START    UPON  CONS.
*
     OPEN        INPUT       YTINYKF
                             NYKRUIF
     OPEN        OUTPUT      YTICHKF.

*システム日付・時刻の取得
     ACCEPT   SYS-DATE          FROM   DATE.
     ACCEPT   SYS-TIME          FROM   TIME.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   WK-DATE8.

     INITIALIZE             WK-AREA.
*
*入荷予定データを読み込む
     PERFORM  YTINYKF-READ-SEC.
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4000     TO    PROGRAM-STATUS
          STOP     RUN
     END-IF.

*
 INIT-EXIT.
     EXIT.
****************************************************************
*    主処理
****************************************************************
 MAIN-SEC                    SECTION.
     MOVE     "MAIN-SEC"          TO   S-NAME.

*入荷累積存在チェック
     MOVE     YTI-F01        TO    NYK-F060.
     MOVE     YTI-F02        TO    NYK-F061.
     PERFORM  NYKRUIF-READ-SEC.
     IF  NYKRUIF-INV-FLG  =  SPACE
         MOVE    SPACE       TO    CHK-REC
         INITIALIZE                CHK-REC
         MOVE    YTI-REC     TO    CHK-REC
         MOVE    NYK-F01     TO    CHK-F98
         MOVE    NYK-F03     TO    CHK-F99
         WRITE   CHK-REC
         ADD     1           TO    CHK-CNT1
     ELSE
         MOVE    SPACE       TO    CHK-REC
         INITIALIZE                CHK-REC
         MOVE    YTI-REC     TO    CHK-REC
         MOVE    SPACE       TO    CHK-F98
         MOVE    ZERO        TO    CHK-F99
         WRITE   CHK-REC
         ADD     1           TO    CHK-CNT2
     END-IF.

 MAIN-900.
*次の入荷予定データを読み込む
     PERFORM  YTINYKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*    入荷予定データ　順読込
****************************************************************
 YTINYKF-READ-SEC            SECTION.
     MOVE     "YTINYKF-READ-SEC"  TO   S-NAME.
*
     READ     YTINYKF
         AT END
              MOVE   "END"        TO   END-FLG
              GO      TO          YTINYKF-READ-EXIT
     END-READ.
*
     ADD      1   TO    YTI-CNT.
*
 YTINYKF-READ-EXIT.
     EXIT.
****************************************************************
*    入荷累積ファイル　　読込み
****************************************************************
 NYKRUIF-READ-SEC           SECTION.
     MOVE     "NYKRUIF-READ-SEC" TO    S-NAME.
*
     MOVE     SPACE              TO    NYKRUIF-INV-FLG.

     READ      NYKRUIF
          INVALID
               MOVE   "INV"      TO    NYKRUIF-INV-FLG
     END-READ.
*
 NYKRUIF-READ-EXIT.
     EXIT.
****************************************************************
*    終了処理
****************************************************************
 END-SEC                   SECTION.
     MOVE     "END-SEC"         TO   S-NAME.
*
     DISPLAY "YRTI-CNT = " YTI-CNT   UPON  CONS.
     DISPLAY "CHK-CNT1 = " CHK-CNT1 UPON  CONS.
     DISPLAY "CHK-CNT2 = " CHK-CNT2 UPON  CONS.
     DISPLAY  MSG-OUT2        UPON   CONS.
*
     CLOSE    YTINYKF
              NYKRUIF
              YTICHKF.

 END-EXIT.
     EXIT.
