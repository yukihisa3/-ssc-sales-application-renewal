# SZA9997B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SZA9997B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　社内振替　　　                    *
*    モジュール名　　　　：　在庫マスタ未入庫数計算：社内振替分*
*    作成日／作成者　　　：　2018/05/11 NAV TAKAHASHI          *
*    処理概要　　　　　　：　社内振替明細、情報を読み、未完了の*
*      　　　　　　　　　　　発注数、入庫数を未入庫数に更新する*
*　　更新日／更新者　　　：　2018/11/01 NAV TAKAHASHI          *
*    修正概要　　　　　　：　未入庫数の更新方法を変更　        *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：　　　　　　　　　　　　　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SZA9997B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2018/05/11.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振替明細ファイル
     SELECT   SFRMEIL1  ASSIGN    TO        DA-01-VI-SFRMEIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       MEI-F01
                                            MEI-F02
                                            MEI-F03
                                            MEI-F04
                                            MEI-F05
                                            MEI-F06
                                            MEI-F07
                                            MEI-F08
                                            MEI-F11
                                            MEI-F12
                        FILE  STATUS   IS   MEI-STATUS.
*振替情報ファイル
     SELECT   SFRHEDL1  ASSIGN    TO        DA-01-VI-SFRHEDL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HED-F01
                                            HED-F02
                                            HED-F03
                                            HED-F04
                                            HED-F05
                                            HED-F06
                                            HED-F07
                                            HED-F08
                        FILE  STATUS   IS   HED-STATUS.
*商品在庫マスタ
     SELECT   ZAMZAIL1  ASSIGN    TO        DA-01-VI-ZAMZAIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       ZAI-F01
                                            ZAI-F021
                                            ZAI-F022
                                            ZAI-F03
                        FILE      STATUS    ZAI-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MES-F011
                                            MES-F0121
                                            MES-F0122
                                            MES-F0123
                        FILE      STATUS    MES-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    振替明細ファイル
******************************************************************
 FD  SFRMEIL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRMEIL1  OF        XFDLIB
              JOINING   MEI  AS   PREFIX.
*
******************************************************************
*    振替情報ファイル
******************************************************************
 FD  SFRHEDL1
                        LABEL RECORD   IS   STANDARD.
     COPY     SFRHEDL1  OF        XFDLIB
              JOINING   HED  AS   PREFIX.
******************************************************************
*    商品在庫マスタ
******************************************************************
 FD  ZAMZAIL1.
     COPY        ZAMZAIF   OF        XFDLIB
                 JOINING   ZAI       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1.
     COPY        HMEIMS    OF        XFDLIB
                 JOINING   MES       PREFIX.
*
*****************************************************************
 WORKING-STORAGE        SECTION.
*    ﾜｰｸ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  REWT1-CNT               PIC  9(08)     VALUE  ZERO.
 01  REWT2-CNT               PIC  9(08)     VALUE  ZERO.
 01  WT1-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WT2-CNT                 PIC  9(08)     VALUE  ZERO.
 01  SFRHEDL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  ZAMZAIL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  MEIMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
*
 01  WK-ST.
     03  MEI-STATUS        PIC  X(02).
     03  HED-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  MES-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SZA9997B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SZA9997B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SZA9997B".
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
*--------------------------------------------------------*
*日付／時刻
 01  WSYS-DATE.
     03  WSYS-Y1           PIC 9(02).
     03  WSYS-YMD.
         05  WSYS-YY       PIC 9(02).
         05  WSYS-MM       PIC 9(02).
         05  WSYS-DD       PIC 9(02).
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*--------------------------------------------------------*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*--------------------------------------------------------*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRMEIL1.
     MOVE      "SFRMEIL1"   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRHEDL1.
     MOVE      "SFRHEDL1"   TO   AB-FILE.
     MOVE      HED-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZAMZAIL1.
     MOVE      "ZAMZAIL1"   TO   AB-FILE.
     MOVE      ZAI-STATUS   TO   AB-STS.
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
     MOVE      MES-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SFRMEIL1  SFRHEDL1   MEIMS1.
     OPEN     I-O       ZAMZAIL1.
     DISPLAY  MSG-START UPON CONS.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     MOVE     ZERO      TO        END-FLG   RD-CNT.
     MOVE     ZERO      TO        REWT1-CNT REWT2-CNT
                                  WT1-CNT   WT2-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*社内振替明細情報ファイル読込
     PERFORM  SFRMEIL1-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　社内振替明細情報ファイル読込　　　　　　　　　　*
****************************************************************
 SFRMEIL1-READ-SEC     SECTION.
*
     READ     SFRMEIL1
              AT END    MOVE     "END"   TO  END-FLG
                        GO               TO  SFRMEIL1-READ-EXIT
              NOT AT END
                        ADD       1      TO  RD-CNT
     END-READ.
*
     IF  RD-CNT(6:3)  =  "000"  OR  "500"
         DISPLAY "READ-CNT = " RD-CNT    UPON  CONS
     END-IF.
*****IF  MEI-F05 = "00906365"
*        DISPLAY "##################" UPON CONS
*    END-IF.
*    IF  MEI-F05 = "00906365"
*        DISPLAY "MEI-F01 = " MEI-F01 UPON CONS
*        DISPLAY "MEI-F02 = " MEI-F02 UPON CONS
*        DISPLAY "MEI-F03 = " MEI-F03 UPON CONS
*        DISPLAY "MEI-F04 = " MEI-F04 UPON CONS
*        DISPLAY "MEI-F05 = " MEI-F05 UPON CONS
*        DISPLAY "MEI-F06 = " MEI-F06 UPON CONS
*        DISPLAY "MEI-F07 = " MEI-F07 UPON CONS
*        DISPLAY "MEI-F08 = " MEI-F08 UPON CONS
*        DISPLAY "MEI-F08 = " MEI-F08 UPON CONS
*        DISPLAY "MEI-F13 = " MEI-F13 UPON CONS
*        DISPLAY "ZAI-F26 = " ZAI-F26 UPON CONS
*****END-IF.
*
 SFRMEIL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　社内振替情報ファイル読込　　　　　　　　　　　　*
****************************************************************
 SFRHEDL1-READ-SEC     SECTION.
*
     READ     SFRHEDL1
              INVALID      MOVE  "INV"   TO  SFRHEDL1-INV-FLG
              NOT  INVALID MOVE  SPACE   TO  SFRHEDL1-INV-FLG
     END-READ.
*
 SFRHEDL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　在庫マスタ読込　　　　　　　　　　　　　　　　　*
****************************************************************
 ZAMZAIL1-READ-SEC     SECTION.
*
     READ     ZAMZAIL1
              INVALID      MOVE  "INV"   TO  ZAMZAIL1-INV-FLG
              NOT  INVALID MOVE  SPACE   TO  ZAMZAIL1-INV-FLG
     END-READ.
*
 ZAMZAIL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　商品名称マスタ読込　　　　　　　　　　　　　　　*
****************************************************************
 MEIMS1-READ-SEC       SECTION.
*
     READ     MEIMS1
              INVALID      MOVE  "INV"   TO  MEIMS1-INV-FLG
              NOT  INVALID MOVE  SPACE   TO  MEIMS1-INV-FLG
     END-READ.
*
 MEIMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*社内振替情報ファイル読込
     MOVE     MEI-F01             TO   HED-F01.
     MOVE     MEI-F02             TO   HED-F02.
     MOVE     MEI-F03             TO   HED-F03.
     MOVE     MEI-F04             TO   HED-F04.
     MOVE     MEI-F05             TO   HED-F05.
     MOVE     MEI-F06             TO   HED-F06.
     MOVE     MEI-F07             TO   HED-F07.
     MOVE     MEI-F08             TO   HED-F08.
     PERFORM  SFRHEDL1-READ-SEC.
     IF  SFRHEDL1-INV-FLG = "INV"
         GO                       TO   MAIN-010
     ELSE
         IF  HED-F24  =  "1"
             GO                   TO   MAIN-010
*#2018/11/01 NAV ST
         ELSE
*************#発注数＜＝入荷数の場合は更新対象としない
             IF  HED-F15 <= HED-F19
                 GO               TO   MAIN-010
             END-IF
         END-IF
*#2018/11/01 NAV ED
     END-IF.
*在庫マスタ索引
     MOVE     MEI-F04             TO    ZAI-F01.
     MOVE     MEI-F05             TO    ZAI-F021.
     MOVE     MEI-F06             TO    ZAI-F022(1:5).
     MOVE     MEI-F07             TO    ZAI-F022(6:2).
     MOVE     MEI-F08             TO    ZAI-F022(8:1).
     MOVE     MEI-F13             TO    ZAI-F03.
     PERFORM  ZAMZAIL1-READ-SEC.
*商品名称マスタ索引
     MOVE     MEI-F05             TO    MES-F011.
     MOVE     MEI-F06             TO    MES-F0121.
     MOVE     MEI-F07             TO    MES-F0122.
     MOVE     MEI-F08             TO    MES-F0123.
     PERFORM  MEIMS1-READ-SEC.
*更新判定
*****在庫マスタが存在した場合
     IF   ZAMZAIL1-INV-FLG  =  SPACE
          IF   MEI-F11  =  "1"
               ADD     MEI-F14    TO    ZAI-F26
***************IF  MEI-F05 = "00906365"
*                  DISPLAY "1 ZAI-F26 = " ZAI-F26 UPON CONS
*                  DISPLAY "1 MEI-F14 = " MEI-F14 UPON CONS
***************END-IF
               MOVE    SYS-DATE   TO    ZAI-F99
               REWRITE ZAI-REC
               ADD     1          TO    REWT1-CNT
          ELSE
***************IF  MEI-F05 = "00906365"
*                  DISPLAY "15ZAI-F26 = " ZAI-F26 UPON CONS
***************END-IF
               COMPUTE ZAI-F26 = ZAI-F26 - MEI-F14
***************IF  MEI-F05 = "00906365"
*                  DISPLAY "2 ZAI-F26 = " ZAI-F26 UPON CONS
*                  DISPLAY "2 MEI-F14 = " MEI-F14 UPON CONS
***************END-IF
*#2018/11/01 NAV ST
               IF  ZAI-F26  <  0
                   MOVE ZERO      TO    ZAI-F26
               END-IF
*#2018/11/01 NAV ST
               MOVE    SYS-DATE   TO    ZAI-F99
               REWRITE ZAI-REC
               ADD     1          TO    REWT2-CNT
          END-IF
     ELSE
          MOVE         SPACE      TO    ZAI-REC
          INITIALIZE                    ZAI-REC
          MOVE         MEI-F04    TO    ZAI-F01
          MOVE         MEI-F05    TO    ZAI-F021
          MOVE         MEI-F06    TO    ZAI-F022(1:5)
          MOVE         MEI-F07    TO    ZAI-F022(6:2)
          MOVE         MEI-F08    TO    ZAI-F022(8:1)
          MOVE         MEI-F13    TO    ZAI-F03
          IF   MEI-F11  =  "1"
               ADD     MEI-F14    TO    ZAI-F26
               ADD     1          TO    WT1-CNT
          ELSE
               COMPUTE ZAI-F26 = ZAI-F26 - MEI-F14
*#2018/11/01 NAV ST
               IF  ZAI-F26  <  0
                   MOVE ZERO      TO    ZAI-F26
               END-IF
*#2018/11/01 NAV ED
               ADD     1          TO    WT2-CNT
          END-IF
          IF   MEIMS1-INV-FLG  =  SPACE
               MOVE  MES-F031     TO    ZAI-F30
          ELSE
               MOVE  SPACE        TO    ZAI-F30
          END-IF
          MOVE       SYS-DATE     TO    ZAI-F98
          WRITE  ZAI-REC
     END-IF.
*
 MAIN-010.
*社内振替明細情報ファイル読込
     PERFORM  SFRMEIL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY NC"読込件数　　" " = "  RD-CNT     UPON  CONS.
     DISPLAY NC"発注更新件数" " = "  REWT1-CNT  UPON  CONS.
     DISPLAY NC"入荷更新件数" " = "  REWT2-CNT  UPON  CONS.
     DISPLAY NC"発注作成件数" " = "  WT1-CNT    UPON  CONS.
     DISPLAY NC"入荷作成件数" " = "  WT2-CNT    UPON  CONS.
*
     CLOSE     SFRMEIL1  SFRHEDL1  ZAMZAIL1  MEIMS1.
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
