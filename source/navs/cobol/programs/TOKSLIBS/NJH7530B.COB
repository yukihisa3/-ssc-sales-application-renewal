****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　オンラインデータ作成　　　　　　　*
*    作成日／更新日　　　：　15/12/02                          *
*    作成者／更新者　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*    複数仕入先対応　ＤＣＭサンワ植物（支払）：３９２４　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH7530B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/12/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*店舗マスタ
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*ＤＣＭサンワ支払明細ファイル
     SELECT   SWSIHAF   ASSIGN    TO        DA-01-VI-SWSIHAL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SIH-F07
                                            SIH-F01  SIH-F02
                                            SIH-F03  SIH-F06
                                            SIH-F04
                        FILE  STATUS   IS   SIH-STATUS.
*条件ファイル
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01  JYO-F02
                        FILE  STATUS   IS   JYO-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　１２８　  ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      13   RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  DEN-REC.
     03  DEN-01                  OCCURS  2.
         05  DEN-01A             PIC  X(01).
         05  DEN-01B             PIC  X(127).
     03  DEN-02                  PIC  X(44).
******************************************************************
*    店舗マスタ
******************************************************************
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ＤＣＭサンワ支払明細ファイル
******************************************************************
 FD  SWSIHAF
                        LABEL RECORD   IS   STANDARD.
     COPY     SWSIHAF   OF        XFDLIB
              JOINING   SIH  AS   PREFIX.
******************************************************************
*    条件ファイル
******************************************************************
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*受信データ格納
     COPY   SWSIHAF  OF XFDLIB  JOINING   SWW  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  IDY                     PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SIH-CNT                 PIC  9(08)     VALUE  ZERO.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  CHK-FLG                 PIC  X(01)     VALUE  SPACE.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SIH-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*明細行退避
 01  WK-MEISAI-BODY.
     03  MEISAI-BODY       OCCURS  6.
         05  MEISAI01      PIC  X(06).
         05  MEISAI02      PIC  9(08).
         05  MEISAI03      PIC  9(02).
         05  MEISAI04      PIC  X(112).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH7530B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7530B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH7530B".
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
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMNSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*連番採番
 01  WK-RENBAN.
     03  WK-DEN-NO      PIC  9(10)     VALUE  ZERO.
     03  WK-DEN-ST      PIC  9(10)     VALUE  ZERO.
     03  WK-DEN-ED      PIC  9(10)     VALUE  ZERO.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SWSIHAF.
     MOVE      "SWSIHAL1"   TO   AB-FILE.
     MOVE      SIH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE      "JYOKEN1"    TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"         TO        S-NAME.
     ACCEPT   SYS-DATE       FROM DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD.
     IF       LINK-OUT-RET   =    ZERO
         MOVE LINK-OUT-YMD   TO   SYS-DATEW
     ELSE
         MOVE ZERO           TO   SYS-DATEW
     END-IF.
*
     ACCEPT   SYS-TIME       FROM TIME.
*
     OPEN     INPUT     CVCSG001  TENMS1.
     OPEN     I-O       SWSIHAF   HJYOKEN.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        SWW-REC   CHK-FLG.
     INITIALIZE                   SWW-REC.
 INIT-040.
*条件ファイル　連番　取得
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
         DISPLAY NC"＃＃連番採番値取得エラー！！" UPON CONS
         STOP  RUN
     ELSE
         MOVE  JYO-F04  TO        WK-DEN-NO
         MOVE  JYO-F05  TO        WK-DEN-ST
         MOVE  JYO-F06  TO        WK-DEN-ED
         REWRITE JYO-REC
     END-IF.
*
 INIT-050.
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO        S-NAME.
*
     PERFORM  VARYING   IDX      FROM      1  BY  1
              UNTIL     IDX      >        2
*
*伝票ヘッダ処理
         IF    DEN-01A(IDX)    =    "B"
               MOVE      SPACE       TO    SWW-REC
               INITIALIZE                  SWW-REC
               MOVE      DEN-01(IDX) TO    SWW-F100
          END-IF
*明細行
          IF   DEN-01A(IDX)    =    "D"
               MOVE      SPACE       TO    SWW-F200
               INITIALIZE                  SWW-F200
               MOVE      DEN-01(IDX) TO    SWW-F200
               MOVE      SWW-F104    TO    SWW-F01
               MOVE      SWW-F106    TO    SWW-F03
               MOVE      SWW-F107    TO    SWW-F04
               MOVE      SWW-F116    TO    SWW-F05
               MOVE      SWW-F108    TO    SWW-F06
               ADD       1           TO    WK-DEN-NO
               IF   WK-DEN-NO > WK-DEN-ED
                    MOVE WK-DEN-ST   TO    WK-DEN-NO
               END-IF
               MOVE      WK-DEN-NO   TO    SWW-F07
               MOVE      SYS-DATEW   TO    SWW-F08
               MOVE      SYS-HHMNSS  TO    SWW-F09
               PERFORM   EDIT-SEC
               MOVE      SWW-REC     TO    SIH-REC
               WRITE     SIH-REC
               ADD       1           TO    SIH-CNT
          END-IF
     END-PERFORM.
*
     READ   CVCSG001
         AT END
            MOVE     9           TO    END-FG
         NOT AT END
            ADD      1           TO    RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"          TO        S-NAME.
*取引先ＣＤセット
     MOVE        SPACE           TO        TEN-REC.
     INITIALIZE                            TEN-REC.
     MOVE        SWW-F103        TO        TEN-F52.
     MOVE        SWW-F106        TO        TEN-F011.
     READ    TENMS1
       INVALID     MOVE  "INV"   TO        HTENMS-INV-FLG
       NOT INVALID MOVE  SPACE   TO        HTENMS-INV-FLG
     END-READ.
*計上取引先判定
     IF  HTENMS-INV-FLG = SPACE
         IF      TEN-F73 = SPACE
                 MOVE     3924   TO        SWW-F02
         ELSE
                 MOVE     39241  TO        SWW-F02
         END-IF
     ELSE
         MOVE     3924           TO        SWW-F02
     END-IF.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 HJYOKEN-READ-SEC      SECTION.
*
     MOVE   84           TO    JYO-F01.
     MOVE   "DCMSANWA"   TO    JYO-F02.
     READ   HJYOKEN
            INVALID      MOVE  "INV"    TO   HJYOKEN-INV-FLG
            NOT  INVALID MOVE  SPACE    TO   HJYOKEN-INV-FLG
     END-READ.
*
 HJYOKEN-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"          TO        S-NAME.
*条件ファイル　連番更新
     PERFORM  HJYOKEN-READ-SEC.
     IF  HJYOKEN-INV-FLG = "INV"
         DISPLAY NC"＃＃連番採番値取得エラー！！" UPON CONS
         STOP  RUN
     ELSE
         MOVE  WK-DEN-NO  TO      JYO-F04
         REWRITE  JYO-REC
     END-IF.
*
     MOVE      RD-CNT            TO        IN-CNT.
     MOVE      SIH-CNT           TO        OUT-CNT.
     DISPLAY   MSG-IN            UPON      CONS.
     DISPLAY   MSG-OUT           UPON      CONS.
     DISPLAY   MSG-END           UPON      CONS.
*
     CLOSE     CVCSG001  SWSIHAF  TENMS1   HJYOKEN.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
