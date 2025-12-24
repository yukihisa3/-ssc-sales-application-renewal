****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品業務　　　　　　　　　*
*    モジュール名　　　　：　出荷制限商品ワークチェック　　　　*
*    作成日／更新日　　　：　2015/10/08                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　ワークＦ内の店舗ＣＤ、相手商品Ｃ  *
*                        ：　Ｄのマスタチェックを行う。　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0010C.
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
                        STATUS         SYUSEGF-ST.
*----<< 商品変換ＴＢＬ               >>--*
     SELECT   HSHOTBL   ASSIGN         DA-01-VI-SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TBL-F01
                                       TBL-F02
                        STATUS         HSHOTBL-ST.
*----<< 店舗マスタ                   >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         HTENMS-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷制限商品ワーク           >>--*
 FD  SYUSGWF            LABEL     RECORD   IS   STANDARD.
     COPY     SYUSGWF   OF        XFDLIB
              JOINING   SGW       PREFIX.
*----<< 商品変換ＴＢＬ               >>--*
 FD  HSHOTBL            LABEL     RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
*----<< 店舗マスタ                   >>--*
 FD  HTENMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SYUSEGF-ST              PIC  X(02).
 01  HSHOTBL-ST              PIC  X(02).
 01  HTENMS-ST               PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  HSHOTBL-INV-FLG         PIC  X(03)  VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)  VALUE  SPACE.
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  ERR-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WK-TORICD               PIC  9(08)     VALUE  ZERO.
 01  WK-DATE                 PIC  9(08)     VALUE  ZERO.
 01  WK-TIME                 PIC  9(08)     VALUE  ZERO.
 01  WK-KENSU                PIC  9(06)     VALUE  ZERO.
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK0010C".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0010C".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0010C".
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
     03  MSG-ERR.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " ERR   = ".
         05  CNT-ERR        PIC   9(06).
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
 01  PARA-ERR               PIC   X(01).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-KENSU
                                        PARA-ERR.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷制限商品ワーク           >>--*
 SYUSGWF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGWF.
     MOVE      "SYUSEGL1"   TO   AB-FILE.
     MOVE      SYUSEGF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 商品変換ＴＢＬ               >>--*
 HSHOTBL-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      HSHOTBL-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 店舗マスタ                   >>--*
 HTENMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      HTENMS-ST    TO   AB-STS.
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
     OPEN     I-O       SYUSGWF.
     OPEN     INPUT     HSHOTBL  HTENMS.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FLG   RD-CNT    WRT-CNT.
*----<< 出荷制限商品ワーク読込          >>--*
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
*----<< エラーチェック  >>--*
*----店舗マスタ存在チェック
     PERFORM  900-TEN-READ.
     IF  HTENMS-INV-FLG = "INV"
         MOVE  "1"            TO   SGW-F08
     ELSE
         MOVE  SPACE          TO   SGW-F08
     END-IF.
*----商品変換ＴＢＬ存在チェック
     PERFORM  900-TBL-READ.
     IF  HSHOTBL-INV-FLG = "INV"
         MOVE  "1"            TO   SGW-F09
     ELSE
         MOVE  SPACE          TO   SGW-F09
     END-IF.
*----エラーがあった場合、取込エラー区分にも１セット
     IF  SGW-F08  =  "1"
     OR  SGW-F09  =  "1"
         MOVE  "1"            TO   SGW-F10
         ADD    1             TO   ERR-CNT
     ELSE
         GO                   TO   MAIN-010
     END-IF.
*----出荷制限商品ワーク更新
     REWRITE  SGW-REC.
     ADD      1               TO   WRT-CNT.
*----<< 出荷制限商品ワーク読込          >>--*
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
     MOVE      ERR-CNT   TO      CNT-ERR.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-ERR   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     IF  ERR-CNT  >  ZERO
         MOVE  "E"              TO   PARA-ERR
     END-IF.
*
*----<<FILE CLOSE >>--*
     CLOSE     SYUSGWF.
     CLOSE     HSHOTBL  HTENMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＥＤＩＣ支払ＭＳＧ　 READ                    *
*--------------------------------------------------------------*
 900-SGW-READ           SECTION.
*
     MOVE     "900-SGW-READ"      TO   S-NAME.
*
     READ     SYUSGWF
       AT END
           MOVE      "END"        TO   END-FLG
       NOT AT END
           ADD       1            TO   RD-CNT
     END-READ.
*
 900-SGW-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品変換ＴＢＬ索引
*--------------------------------------------------------------*
 900-TBL-READ           SECTION.
*
     MOVE     "900-TBL-READ"      TO   S-NAME.
*
     MOVE      SGW-F01            TO   TBL-F01.
     MOVE      SGW-F03            TO   TBL-F02.
     READ      HSHOTBL
           INVALID
           MOVE      "INV"        TO   HSHOTBL-INV-FLG
           NOT  INVALID
           MOVE      SPACE        TO   HSHOTBL-INV-FLG
     END-READ.
*
 900-TBL-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    店舗マスタ索引
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
*
     MOVE     "900-TEN-READ"      TO   S-NAME.
*
     MOVE      SGW-F01            TO   TEN-F52.
     MOVE      SGW-F02            TO   TEN-F011.
     READ      HTENMS
           INVALID
           MOVE      "INV"        TO   HTENMS-INV-FLG
           NOT  INVALID
           MOVE      SPACE        TO   HTENMS-INV-FLG
     END-READ.
*
 900-TEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
