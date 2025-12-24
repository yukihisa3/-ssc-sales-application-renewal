****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品業務　　　　　　　　　*
*    モジュール名　　　　：　出荷制限マスタ作成　　　　　　　　*
*    作成日／更新日　　　：　2015/10/12                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　出荷制限商品ワークを読み、出荷　  *
*                        ：　制限マスタを作成する。　　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0010B.
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
*----<< 出荷制限商品ワーク           >>--*
     SELECT   SYUSGWF   ASSIGN         DA-01-VI-SYUSGWL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SGW-F01
                                       SGW-F02
                                       SGW-F03
                        STATUS         SYUSGWF-ST.
*----<< 出荷制限マスタ　　　         >>--*
     SELECT   SYUSGNF   ASSIGN         DA-01-VI-SYUSGNL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SGN-F01
                                       SGN-F02
                                       SGN-F03
                        STATUS         SYUSGNF-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷制限商品ワーク           >>--*
 FD  SYUSGWF            LABEL     RECORD   IS   STANDARD.
     COPY     SYUSGWF   OF        XFDLIB
              JOINING   SGW       PREFIX.
*----<< 出荷制限マスタ               >>--*
 FD  SYUSGNF            LABEL     RECORD   IS   STANDARD.
     COPY     SYUSGNF   OF        XFDLIB
              JOINING   SGN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SYUSGWF-ST              PIC  X(02).
 01  SYUSGNF-ST              PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  SYUSGNF-INV-FLG         PIC  X(03)  VALUE  SPACE.
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(08)     VALUE  ZERO.
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0010B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0010B".
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
 01  PARA-KENSU             PIC   9(06).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷制限商品ワーク           >>--*
 SYUSGWF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGWF.
     MOVE      "SYUSGWL1"   TO   AB-FILE.
     MOVE      SYUSGWF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 出荷制限マスタ               >>--*
 SYUSGNF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGNF.
     MOVE      "SYUSGNL1"   TO   AB-FILE.
     MOVE      SYUSGNF-ST   TO   AB-STS.
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
     OPEN     INPUT     SYUSGWF.
     OPEN     I-O       SYUSGNF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FLG   RD-CNT    WRT-CNT.
*----<< 出荷制限商品ワーク読込 >>--*
     PERFORM  900-SGW-READ.
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
*----<< 出荷制限マスタ存在チェック  >>--*
     PERFORM 900-SGN-READ.
     IF  SYUSGNF-INV-FLG  =  SPACE
         ADD     1            TO   SKIP-CNT
         GO                   TO   MAIN-010
     END-IF.
*----<< 出荷制限マスタ出力 >>--*
     MOVE  SPACE              TO   SGN-REC.
     INITIALIZE                    SGN-REC.
*----取引先コード
     MOVE     SGW-F01         TO   SGN-F01.
*----店舗コード
     MOVE     SGW-F02         TO   SGN-F02.
*----相手先商品コード
     MOVE     SGW-F03         TO   SGN-F03.
*----相手先商品名カナ
     MOVE     SGW-F04         TO   SGN-F04.
*----作成日
     MOVE     SGW-F05         TO   SGN-F05.
*----作成時刻
     MOVE     SGW-F06         TO   SGN-F06.
     MOVE     "00"            TO   SGN-F06(5:2).
*----作成件数
     MOVE     PARA-KENSU      TO   SGN-F07.
*----登録担当者コード
     MOVE     SGW-F92         TO   SGN-F92.
*----登録担当者部門コード
     MOVE     SGW-F93         TO   SGN-F93.
*----登録日付
     MOVE     SYS-DATEW       TO   SGN-F94.
*----登録時刻
     MOVE     SYS-TIME(1:6)   TO   SGN-F95.
*----出力処理
     WRITE    SGN-REC.
     ADD      1               TO   WRT-CNT.
*
*----<< 出荷制限商品ワーク読込 >>--*
 MAIN-010.
     PERFORM  900-SGW-READ.

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
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*----<<FILE CLOSE >>--*
     CLOSE     SYUSGWF.
     CLOSE     SYUSGNF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    出荷制限商品ワーク読込
*--------------------------------------------------------------*
 900-SGW-READ           SECTION.
*
     MOVE     "900-SGW-READ"      TO   S-NAME.
*
     READ     SYUSGWF
       AT END
           MOVE      "END"        TO   END-FLG
           GO                     TO   900-SGW-READ-EXIT
       NOT AT END
           ADD       1            TO   RD-CNT
     END-READ.
*
     IF  RD-CNT(6:3) = "000" OR "500"
         DISPLAY "## READ-CNT = " RD-CNT " ##" UPON CONS
     END-IF.
*エラーの場合は更新しない。また、異常終了させる。
     IF  SGW-F10  =  "1"
         DISPLAY NC"＃データ内にエラーが存在！！＃" UPON CONS
         DISPLAY NC"＃相手商品ＣＤ＝" SGW-F03 " " NC"＃"
                                                    UPON CONS
         MOVE  4010               TO   PROGRAM-STATUS
         STOP  RUN
     END-IF.
*
 900-SGW-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    出荷制限マスタ存在チェック処理
*--------------------------------------------------------------*
 900-SGN-READ           SECTION.
*
     MOVE     "900-SGN-READ"      TO   S-NAME.
*
     MOVE      SGW-F01            TO   SGN-F01.
     MOVE      SGW-F03            TO   SGN-F02.
     MOVE      SGW-F02            TO   SGN-F03.
     READ      SYUSGNF
           INVALID
           MOVE      "INV"        TO   SYUSGNF-INV-FLG
           NOT  INVALID
           MOVE      SPACE        TO   SYUSGNF-INV-FLG
     END-READ.
*
 900-SGN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
