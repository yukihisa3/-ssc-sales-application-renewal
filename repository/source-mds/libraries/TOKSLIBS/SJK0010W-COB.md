# SJK0010W

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SJK0010W.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品業務　　　　　　　　　*
*    モジュール名　　　　：　出荷制限商品ワーク作成　　　　　　*
*    作成日／更新日　　　：　2015/10/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　取込んだＣＳＶデータを、ワークＦ  *
*                        ：　ヘの取込みを行う。また、作成日、  *
*                        ：　作成時刻、件数をパラアウトする。  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0010W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/10/08.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 出荷制限商品データ（ＣＳＶ） >>--*
     SELECT   SYUSEGWK  ASSIGN         DA-01-S-SYUSEGWK
                        STATUS         SYUSEGWK-ST.
*----<< 出荷制限商品ワーク           >>--*
     SELECT   SYUSGWF   ASSIGN         DA-01-VI-SYUSGWL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEW-F01
                                       SEW-F02
                                       SEW-F03
                        STATUS         SYUSEGF-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷制限商品データ（ＣＳＶ） >>--*
 FD  SYUSEGWK                                                             
                        BLOCK    CONTAINS  58   RECORDS                   
                        LABEL    RECORD    IS   STANDARD.                 
                        COPY     SYUSEGWK  OF   XFDLIB                    
                        JOINING  SEG       AS   PREFIX.                   
*----<< 出荷制限商品ワーク           >>--*
 FD  SYUSGWF            LABEL     RECORD   IS   STANDARD.
     COPY     SYUSGWF   OF        XFDLIB
              JOINING   SEW       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SYUSEGWK-ST             PIC  X(02).
 01  SYUSEGF-ST              PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  SYUSGWF-INV-FLG         PIC  X(03)  VALUE  SPACE.
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(08)     VALUE  ZERO.
 01  WK-TORICD               PIC  9(08)     VALUE  ZERO.
 01  WK-DATE                 PIC  9(08)     VALUE  ZERO.
 01  WK-TIME                 PIC  9(04)     VALUE  ZERO.
 01  WK-KENSU                PIC  9(06)     VALUE  ZERO.
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK0010W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0010W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0010W".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-SKIP.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " SKIP  = ".
         05  CNT-SKIP       PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-DATEW          PIC  9(08).
 01  FILLER             REDEFINES      SYS-DATEW.
     03  SYS-YYW        PIC  9(04).
     03  SYS-MMW        PIC  9(02).
     03  SYS-DDW        PIC  9(02).
 01  SYS-TIME           PIC  9(08).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
 01  PARA-KENSU             PIC   9(06).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-BUMON
                                        PARA-TANCD
                                        PARA-TORICD
                                        PARA-DATE
                                        PARA-TIME
                                        PARA-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷制限商品データ（ＣＳＶ） >>--*
 SYUSEGWK-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSEGWK.
     MOVE      "SYUSEGWK"   TO   AB-FILE.
     MOVE      SYUSEGWK-ST  TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 出荷制限商品ワーク           >>--*
 SYUSGWF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGWF.
     MOVE      "SYUSGWL1"   TO   AB-FILE.
     MOVE      SYUSEGF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
*----<< ｼｮｷﾁ ｾｯﾄ >--*
     MOVE     "INIT-SEC"          TO   S-NAME.
*----<< システム日付取得  >>--*
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD  TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*----<< システム時刻取得  >>--*
     ACCEPT   SYS-TIME       FROM TIME.
*
*----<< ｼｮｷﾒｯｾｰｼﾞ ｼｭﾂﾘｮｸ >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     INPUT     SYUSEGWK.
     OPEN     I-O       SYUSGWF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FLG   RD-CNT    WRT-CNT.
*----<< 出荷制限商品データ（ＣＳＶ）読込 >>--*
     PERFORM  900-SEG-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*----<< 出荷制限商品ワーク存在チェック  >>--*
     PERFORM 900-SEW-READ.
     IF  SYUSGWF-INV-FLG  =  SPACE
         ADD     1            TO   SKIP-CNT
         GO                   TO   MAIN-010
     END-IF.
*----<< 出荷制限商品ワーク出力 >>--*
     MOVE  SPACE              TO   SEW-REC.
     INITIALIZE                    SEW-REC.
*----取引先コード
     MOVE     SEG-F01         TO   SEW-F01  WK-TORICD.
*----店舗コード
     MOVE     SEG-F02         TO   SEW-F02.
*----相手先商品コード
     MOVE     SEG-F03         TO   SEW-F03.
*----相手先商品名カナ
     MOVE     SEG-F04         TO   SEW-F04.
*----作成日
     MOVE     SEG-F05         TO   SEW-F05  WK-DATE.
*----作成時刻
     MOVE     SEG-F06         TO   SEW-F06.
     MOVE     SEG-F06(1:4)    TO   WK-TIME.
*----作成件数
     MOVE     SEG-F07         TO   SEW-F07  WK-KENSU.
*----登録担当者コード
     MOVE     PARA-TANCD      TO   SEW-F92.
*----登録担当者部門コード
     MOVE     PARA-BUMON      TO   SEW-F93.
*----登録日付
     MOVE     SYS-DATEW       TO   SEW-F94.
*----登録時刻
     MOVE     SYS-TIME(1:6)   TO   SEW-F95.
*----出力処理
     WRITE    SEW-REC.
     ADD      1               TO   WRT-CNT.
*
*----<< 出荷制限商品データ（ＣＳＶ）読込 >>--*
 MAIN-010.
     PERFORM  900-SEG-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     MOVE      SKIP-CNT  TO      CNT-SKIP.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-SKIP  UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     MOVE      WK-TORICD TO      PARA-TORICD.
     MOVE      WK-DATE   TO      PARA-DATE.
     MOVE      WK-TIME   TO      PARA-TIME.
     DISPLAY "WK-TIME = " WK-TIME UPON CONS.
     MOVE      WK-KENSU  TO      PARA-KENSU.
*----<<FILE CLOSE >>--*
     CLOSE     SYUSEGWK.
     CLOSE     SYUSGWF.
*
     IF  OUT-CNT = ZERO
         MOVE      4001         TO   PROGRAM-STATUS
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＥＤＩＣ支払ＭＳＧ　 READ                    *
*--------------------------------------------------------------*
 900-SEG-READ           SECTION.
*
     MOVE     "900-SEG-READ"      TO   S-NAME.
*
     READ     SYUSEGWK
       AT END
           MOVE      "END"        TO   END-FLG
       NOT AT END
           ADD       1            TO   RD-CNT
     END-READ.
*
 900-SEG-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    出荷制限商品ワーク存在チェック処理
*--------------------------------------------------------------*
 900-SEW-READ           SECTION.
*
     MOVE     "900-SEW-READ"      TO   S-NAME.
*
     MOVE      SEG-F01            TO   SEW-F01.
     MOVE      SEG-F03            TO   SEW-F02.
     MOVE      SEG-F02            TO   SEW-F03.
     READ      SYUSGWF
           INVALID
           MOVE      "INV"        TO   SYUSGWF-INV-FLG
           NOT  INVALID
           MOVE      SPACE        TO   SYUSGWF-INV-FLG
     END-READ.
*
 900-SEW-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
