****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*                                                              *
*    業務名　　　　　　　：　社内振替　　　　　　　　　　　　　*
*    モジュール名　　　　：　社内振替情報チェックリスト　　　  *
*    　　　　　　　　　　　　ＣＳＶデータ出力　　　　　　　　  *
*    作成日／作成者　　　：　2017/01/17 INOUE                  *
*    処理概要　　　　　　：　振分リスト出力ワークより、　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    流用元　　　　　　　：　SSY3910V                          *
*                                                              *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU3100V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/01/17.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振分リストワーク
     SELECT   SFRLSTW1  ASSIGN    TO        DA-01-VI-SFRLSTW1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       LST-F01 LST-F02
                                            LST-F03 LST-F04
                                            LST-F05 LST-F06
                                            LST-F07 LST-F08
                                            LST-F12 LST-F11
                        FILE      STATUS    LST-STATUS.
*振分リストＣＳＶ
     SELECT   SFRCHK    ASSIGN    TO        DA-01-S-SFRCHK
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*倉庫マスタ
     SELECT   ZSOKMS1   ASSIGN     TO      DA-01-VI-ZSOKMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     SOK-F01
                        FILE    STATUS     SOK-STATUS.
*担当者マスタ
     SELECT   TANMS1    ASSIGN     TO      DA-01-VI-TANMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     TAN-F01  TAN-F02
                        FILE    STATUS     TAN-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*振分リストワーク
******************************************************************
 FD  SFRLSTW1           LABEL RECORD   IS   STANDARD.
     COPY     SFRLSTW1  OF        XFDLIB
              JOINING   LST  AS   PREFIX.
*
******************************************************************
*振分リストＣＳＶ
******************************************************************
 FD  SFRCHK            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   9         RECORDS.
     COPY     SFRCHK01   OF        XFDLIB
              JOINING    CSV       PREFIX.
******************************************************************
*倉庫マスタ
******************************************************************
 FD  ZSOKMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS1   OF        XFDLIB
              JOINING   SOK  AS   PREFIX.
*
******************************************************************
*担当者マスタ
******************************************************************
 FD  TANMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     TANMS1    OF        XFDLIB
              JOINING   TAN  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*振分リストＣＳＶマルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     SFRCHK01  OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.指定条件行１
     COPY     SFRCHK02  OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.指定条件行２
     COPY     SFRCHK03  OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
* 4.項目タイトル行１
     COPY     SFRCHK04  OF        XFDLIB
              JOINING   CSV4 AS   PREFIX.
* 5.項目タイトル行２
     COPY     SFRCHK05  OF        XFDLIB
              JOINING   CSV5 AS   PREFIX.
* 6.明細行
     COPY     SFRCHK06  OF        XFDLIB
              JOINING   CSV6 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  LST-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  SOK-STATUS          PIC  X(02).
     03  TAN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SFRLSTW1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  SOKO-HIT-FLG            PIC  X(03)     VALUE  SPACE.
 01  KEY-BRK-FLG             PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*添字
 01  IX                      PIC  9(03)     VALUE  ZERO.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SFU3100V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SFU3100V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SFU3100V".
         05  FILLER          PIC  X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  AB-FILE         PIC  X(08).
         05  FILLER          PIC  X(06)  VALUE " ST = ".
         05  AB-STS          PIC  X(02).
         05  FILLER          PIC  X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  FILLER          PIC  X(07)  VALUE " SEC = ".
         05  S-NAME          PIC  X(30).
     03  MSG-IN.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " 振分情報読込件数   = ".
         05  MSG-IN01        PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT01       PIC  ZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  LINK-IN-OUTSOKO         PIC   X(02).
 01  LINK-IN-DATEF           PIC   9(08).
 01  LINK-IN-DATET           PIC   9(08).
 01  LINK-IN-KANKBN          PIC   X(01).
 01  LINK-IN-DTKBN           PIC   X(01).
 01  LINK-IN-INKBN           PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING LINK-IN-OUTSOKO
                                       LINK-IN-DATEF
                                       LINK-IN-DATET
                                       LINK-IN-KANKBN
                                       LINK-IN-DTKBN
                                       LINK-IN-INKBN.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRLSTW1.
     MOVE      "SFRLSTW1"   TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRCHK.
     MOVE      "SFRCHK"     TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS1.
     MOVE      "ZSOKMS1"   TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1"    TO   AB-FILE.
     MOVE      TAN-STATUS   TO   AB-STS.
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
*
*初期処理
     PERFORM  INIT-SEC.
*明細ＣＳＶ出力
     PERFORM  MAIN-SEC
              UNTIL     END-FLG   =  "END".
*終了処理
     PERFORM  END-SEC.
     STOP  RUN.
*
 GENERAL-PROCESS-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SFRLSTW1  ZSOKMS1  TANMS1.
     OPEN     OUTPUT    SFRCHK.
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
*振分リストワーク読込
     PERFORM SFRLSTW1-READ-SEC.
*終了判定
     IF   SFRLSTW1-END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-IF.
*倉庫名取得
     MOVE LINK-IN-OUTSOKO   TO    SOK-F01.
     READ ZSOKMS1
        INVALID
          DISPLAY "＃＃倉庫マスタ　無し！＃＃" UPON CONS
          DISPLAY "＃　倉庫ＣＤ＝　" LINK-IN-OUTSOKO UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　振分リストワーク読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 SFRLSTW1-READ-SEC    SECTION.
*
     MOVE    "SFRLSTW1-READ-SEC"    TO   S-NAME.
*
     READ     SFRLSTW1
              AT  END
                  MOVE     "END"    TO  SFRLSTW1-END-FLG
                  GO                TO  SFRLSTW1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
 SFRLSTW1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"     TO      S-NAME.
*
 MAIN-01.
     MOVE     SPACE          TO      CSV-REC.
     MOVE     SPACE          TO      CSV1-REC.
     MOVE     SPACE          TO      CSV2-REC.
     MOVE     SPACE          TO      CSV3-REC.
     MOVE     SPACE          TO      CSV4-REC.
     MOVE     SPACE          TO      CSV5-REC.
     MOVE     SPACE          TO      CSV6-REC.
*****INITIALIZE                      SUM-AREA.
*****INITIALIZE                      CSV-REC.
*
*  振分ＣＳＶ SET/OUT  1.帳票タイトル行
     PERFORM  CSV1-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  振分ＣＳＶ SET/OUT  2.指定条件行1
     PERFORM  CSV2-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
*  振分ＣＳＶ SET/OUT  3.指定条件行2
     PERFORM  CSV3-SET-SEC.
     WRITE    CSV-REC   FROM      CSV3-REC.
*
*  振分ＣＳＶ SET/OUT  4.項目タイトル行1
     PERFORM  CSV4-SET-SEC.
     WRITE    CSV-REC   FROM      CSV4-REC.
*
*  振分ＣＳＶ SET/OUT  5.項目タイトル行2
     PERFORM  CSV5-SET-SEC.
     WRITE    CSV-REC   FROM      CSV5-REC.
*
 MAIN-02.
*
*  振分ＣＳＶ SET/OUT  6.明細行
     PERFORM  CSV6-SET-SEC.
*
*    レコード出力
     WRITE    CSV-REC            FROM  CSV6-REC.
     ADD      1                  TO    OUT-CNT.
     MOVE     SPACE              TO    CSV6-REC.
*
*    振分リストワーク読込み
     PERFORM  SFRLSTW1-READ-SEC.
     IF       SFRLSTW1-END-FLG    =    "END"
              MOVE    "END"      TO    END-FLG
              GO                 TO    MAIN-EXIT
     END-IF.
*
     GO       TO       MAIN-02.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   1.帳票タイトル行
****************************************************************
 CSV1-SET-SEC     SECTION.
*
     MOVE   "CSV1-SET-SEC"  TO  S-NAME.
*
 CSV1-SET-01.
*
     MOVE    SPACE                                TO CSV1-REC.
*
     MOVE    X"28"                                TO CSV1-CS00.
     MOVE    NC"社内振替情報チェックリストＣＳＶ" TO CSV1-C00.
     MOVE    X"29"                                TO CSV1-CE00.
     MOVE    ","                                  TO CSV1-CK00.
*
 CSV1-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   2.項目タイトル行１
****************************************************************
 CSV2-SET-SEC     SECTION.
*
     MOVE   "CSV2-SET-SEC"  TO  S-NAME.
*
 CSV2-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-TS101.
     MOVE    NC"指定ＤＴ区分："                   TO CSV2-T101.
     MOVE    X"29"                                TO CSV2-TE101.
     MOVE    ","                                  TO CSV2-TK101.
     MOVE    X"28"                                TO CSV2-TS102.
     IF      LINK-IN-DTKBN  =   " "
             MOVE    NC"全て　　"                 TO CSV2-T102
     END-IF.
     IF      LINK-IN-DTKBN  =   "1"
             MOVE    NC"発注のみ"                 TO CSV2-T102
     END-IF.
     IF      LINK-IN-DTKBN  =   "2"
             MOVE    NC"入荷のみ"                 TO CSV2-T102
     END-IF.
     MOVE    X"29"                                TO CSV2-TE102.
     MOVE    ","                                  TO CSV2-TK102.
*
 CSV2-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   3.項目タイトル行2
****************************************************************
 CSV3-SET-SEC     SECTION.
*
     MOVE   "CSV3-SET-SEC"  TO  S-NAME.
*
 CSV3-SET-01.
*
     MOVE    SPACE                                TO CSV3-REC.
*
     MOVE    X"28"                                TO CSV3-TS201.
     MOVE    NC"指定完納区分："                   TO CSV3-T201.
     MOVE    X"29"                                TO CSV3-TE201.
     MOVE    ","                                  TO CSV3-TK201.
     MOVE    X"28"                                TO CSV3-TS202.
     IF      LINK-IN-KANKBN =   " "
             MOVE    NC"全て　　"                 TO CSV3-T202
     END-IF.
     IF      LINK-IN-KANKBN =   "1"
             MOVE    NC"未完納分"                 TO CSV3-T202
     END-IF.
     IF      LINK-IN-KANKBN =   "2"
             MOVE    NC"完納分　"                 TO CSV3-T202
     END-IF.
     MOVE    X"29"                                TO CSV3-TE202.
     MOVE    ","                                  TO CSV3-TK202.
*
 CSV3-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   4.項目タイトル行１
****************************************************************
 CSV4-SET-SEC     SECTION.
*
     MOVE   "CSV4-SET-SEC"  TO  S-NAME.
*
 CSV4-SET-01.
*
     MOVE    SPACE                                TO CSV4-REC.
*
     MOVE    X"28"                                TO CSV4-TS301.
     MOVE    NC"ヘッダ情報→"                     TO CSV4-T301.
     MOVE    X"29"                                TO CSV4-TE301.
     MOVE    ALL ","                              TO CSV4-TK301.
     MOVE    X"28"                                TO CSV4-TS302.
     MOVE    NC"明細情報→"                       TO CSV4-T302.
     MOVE    X"29"                                TO CSV4-TE302.
     MOVE    ","                                  TO CSV4-TK302.
*
 CSV4-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   5.項目タイトル行２
****************************************************************
 CSV5-SET-SEC     SECTION.
*
     MOVE   "CSV5-SET-SEC"  TO  S-NAME.
*
 CSV5-SET-01.
*
     MOVE    SPACE                                TO CSV5-REC.
*
     MOVE    X"28"                                TO CSV5-KS01.
     MOVE    NC"倉庫ＣＤ"                         TO CSV5-K01.
     MOVE    X"29"                                TO CSV5-KE01.
     MOVE    ","                                  TO CSV5-KK01.
*
     MOVE    X"28"                                TO CSV5-KS02.
     MOVE    NC"倉庫名"                           TO CSV5-K02.
     MOVE    X"29"                                TO CSV5-KE02.
     MOVE    ","                                  TO CSV5-KK02.
*
     MOVE    X"28"                                TO CSV5-KS03.
     MOVE    NC"伝票区分"                         TO CSV5-K03.
     MOVE    X"29"                                TO CSV5-KE03.
     MOVE    ","                                  TO CSV5-KK03.
*
     MOVE    X"28"                                TO CSV5-KS04.
     MOVE    NC"年度"                             TO CSV5-K04.
     MOVE    X"29"                                TO CSV5-KE04.
     MOVE    ","                                  TO CSV5-KK04.
*
     MOVE    X"28"                                TO CSV5-KS05.
     MOVE    NC"シーズン"                         TO CSV5-K05.
     MOVE    X"29"                                TO CSV5-KE05.
     MOVE    ","                                  TO CSV5-KK05.
*
     MOVE    X"28"                                TO CSV5-KS06.
     MOVE    NC"サカタ商品ＣＤ"                   TO CSV5-K06.
     MOVE    X"29"                                TO CSV5-KE06.
     MOVE    ","                                  TO CSV5-KK06.
*
     MOVE    X"28"                                TO CSV5-KS07.
     MOVE    NC"品単１"                           TO CSV5-K07.
     MOVE    X"29"                                TO CSV5-KE07.
     MOVE    ","                                  TO CSV5-KK07.
*
     MOVE    X"28"                                TO CSV5-KS08.
     MOVE    NC"品単２"                           TO CSV5-K08.
     MOVE    X"29"                                TO CSV5-KE08.
     MOVE    ","                                  TO CSV5-KK08.
*
     MOVE    X"28"                                TO CSV5-KS09.
     MOVE    NC"品単３"                           TO CSV5-K09.
     MOVE    X"29"                                TO CSV5-KE09.
     MOVE    ","                                  TO CSV5-KK09.
*
     MOVE    X"28"                                TO CSV5-KS10.
     MOVE    NC"ＪＡＮＣＤ"                       TO CSV5-K10.
     MOVE    X"29"                                TO CSV5-KE10.
     MOVE    ","                                  TO CSV5-KK10.
*
     MOVE    X"28"                                TO CSV5-KS11.
     MOVE    NC"商品名１"                         TO CSV5-K11.
     MOVE    X"29"                                TO CSV5-KE11.
     MOVE    ","                                  TO CSV5-KK11.
*
     MOVE    X"28"                                TO CSV5-KS12.
     MOVE    NC"商品名２"                         TO CSV5-K12.
     MOVE    X"29"                                TO CSV5-KE12.
     MOVE    ","                                  TO CSV5-KK12.
*
     MOVE    X"28"                                TO CSV5-KS13.
     MOVE    NC"仕入先ＣＤ"                       TO CSV5-K13.
     MOVE    X"29"                                TO CSV5-KE13.
     MOVE    ","                                  TO CSV5-KK13.
*
     MOVE    X"28"                                TO CSV5-KS14.
     MOVE    NC"棚番"                             TO CSV5-K14.
     MOVE    X"29"                                TO CSV5-KE14.
     MOVE    ","                                  TO CSV5-KK14.
*
     MOVE    X"28"                                TO CSV5-KS15.
     MOVE    NC"発注数合計"                       TO CSV5-K15.
     MOVE    X"29"                                TO CSV5-KE15.
     MOVE    ","                                  TO CSV5-KK15.
*
     MOVE    X"28"                                TO CSV5-KS16.
     MOVE    NC"入荷数合計"                       TO CSV5-K16.
     MOVE    X"29"                                TO CSV5-KE16.
     MOVE    ","                                  TO CSV5-KK16.
*
     MOVE    X"28"                                TO CSV5-KS17.
     MOVE    NC"ＭＳＧ"                           TO CSV5-K17.
     MOVE    X"29"                                TO CSV5-KE17.
     MOVE    ","                                  TO CSV5-KK17.
*
     MOVE    X"28"                                TO CSV5-KS18.
     MOVE    NC"完納区分"                         TO CSV5-K18.
     MOVE    X"29"                                TO CSV5-KE18.
     MOVE    ","                                  TO CSV5-KK18.
*
     MOVE    X"28"                                TO CSV5-KS19.
     MOVE    NC"完納担当者ＣＤ"                   TO CSV5-K19.
     MOVE    X"29"                                TO CSV5-KE19.
     MOVE    ","                                  TO CSV5-KK19.
*
     MOVE    X"28"                                TO CSV5-KS20.
     MOVE    NC"完納担当者名"                     TO CSV5-K20.
     MOVE    X"29"                                TO CSV5-KE20.
     MOVE    ","                                  TO CSV5-KK20.
*
     MOVE    X"28"                                TO CSV5-KS21.
     MOVE    NC"入力区分"                         TO CSV5-K21.
     MOVE    X"29"                                TO CSV5-KE21.
     MOVE    ","                                  TO CSV5-KK21.
*
     MOVE    X"28"                                TO CSV5-KS22.
     MOVE    NC"発注／入荷区分"                   TO CSV5-K22.
     MOVE    X"29"                                TO CSV5-KE22.
     MOVE    ","                                  TO CSV5-KK22.
*
     MOVE    X"28"                                TO CSV5-KS23.
     MOVE    NC"発注日"                           TO CSV5-K23.
     MOVE    X"29"                                TO CSV5-KE23.
     MOVE    ","                                  TO CSV5-KK23.
*
     MOVE    X"28"                                TO CSV5-KS24.
     MOVE    NC"入荷日"                           TO CSV5-K24.
     MOVE    X"29"                                TO CSV5-KE24.
     MOVE    ","                                  TO CSV5-KK24.
*
     MOVE    X"28"                                TO CSV5-KS25.
     MOVE    NC"棚番"                             TO CSV5-K25.
     MOVE    X"29"                                TO CSV5-KE25.
     MOVE    ","                                  TO CSV5-KK25.
*
     MOVE    X"28"                                TO CSV5-KS26.
     MOVE    NC"発注数"                           TO CSV5-K26.
     MOVE    X"29"                                TO CSV5-KE26.
     MOVE    ","                                  TO CSV5-KK26.
*
     MOVE    X"28"                                TO CSV5-KS27.
     MOVE    NC"入荷数"                           TO CSV5-K27.
     MOVE    X"29"                                TO CSV5-KE27.
     MOVE    ","                                  TO CSV5-KK27.
*
     MOVE    X"28"                                TO CSV5-KS28.
     MOVE    NC"ＭＳＧ"                           TO CSV5-K28.
     MOVE    X"29"                                TO CSV5-KE28.
     MOVE    ","                                  TO CSV5-KK28.
*
 CSV5-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   6.明細行
****************************************************************
 CSV6-SET-SEC     SECTION.
*
     MOVE     "CSV6-SET-SEC"  TO  S-NAME.
*
 CSV6-SET-01.
*
*倉庫ＣＤ
     MOVE    LINK-IN-OUTSOKO  TO   CSV6-M01.
     MOVE    ","              TO   CSV6-MK01.
*倉庫名
     MOVE    X"28"            TO   CSV6-MS02.
     MOVE    SOK-F02          TO   CSV6-M02.
     MOVE    X"29"            TO   CSV6-ME02.
     MOVE    ","              TO   CSV6-MK02.
*伝票区分
     MOVE    LST-F01          TO   CSV6-M03.
     MOVE    ","              TO   CSV6-MK03.
*年度
     MOVE    LST-F02          TO   CSV6-M04.
     MOVE    ","              TO   CSV6-MK04.
*シーズン
     MOVE    LST-F03          TO   CSV6-M05.
     MOVE    ","              TO   CSV6-MK05.
*サカタ商品ＣＤ
     MOVE    LST-F05          TO   CSV6-M06.
     MOVE    ","              TO   CSV6-MK06.
*品単１
     MOVE    LST-F06          TO   CSV6-M07.
     MOVE    ","              TO   CSV6-MK07.
*品単２
     MOVE    LST-F07          TO   CSV6-M08.
     MOVE    ","              TO   CSV6-MK08.
*品単３
     MOVE    LST-F08          TO   CSV6-M09.
     MOVE    ","              TO   CSV6-MK09.
*ＪＡＮＣＤ
     MOVE    LST-F13          TO   CSV6-M10.
     MOVE    ","              TO   CSV6-MK10.
*商品名１
     MOVE    X"28"            TO   CSV6-MS11.
     MOVE    LST-F09          TO   CSV6-M11.
     MOVE    X"29"            TO   CSV6-ME11.
     MOVE    ","              TO   CSV6-MK11.
*商品名２
     MOVE    X"28"            TO   CSV6-MS12.
     MOVE    LST-F10          TO   CSV6-M12.
     MOVE    X"29"            TO   CSV6-ME12.
     MOVE    ","              TO   CSV6-MK12.
*仕入先ＣＤ
     MOVE    LST-F12          TO   CSV6-M13.
     MOVE    ","              TO   CSV6-MK13.
*棚番
     MOVE    LST-F11          TO   CSV6-M14.
     MOVE    ","              TO   CSV6-MK14.
*発注数合計_符号
*    IF      LST-MF11  =    "1"
             IF      LST-F15   <    0
                     MOVE     "-"   TO   CSV6-M151
             ELSE
                     MOVE     " "   TO   CSV6-M151
             END-IF.
*    END-IF.
*発注数合計_数量
*    IF      LST-MF11  =    "1"
             MOVE    LST-F15   TO   CSV6-M152.
*    END-IF.
     MOVE    ","               TO   CSV6-MK15.
*入荷数合計_符号
*    IF      LST-MF11  =    "2"
             IF      LST-F19   <    0
                     MOVE     "-"   TO   CSV6-M161
             ELSE
                     MOVE     " "   TO   CSV6-M161
             END-IF.
*    END-IF.
*入荷数合計_数量
*    IF      LST-MF11  =    "2"
             MOVE    LST-F19   TO   CSV6-M162.
*    END-IF.
     MOVE    ","               TO   CSV6-MK16.
*ＭＳＧ
     MOVE    X"28"             TO   CSV6-MS17.
     MOVE    LST-F23           TO   CSV6-M17.
     MOVE    X"29"             TO   CSV6-ME17.
     MOVE    ","               TO   CSV6-MK17.
*完納区分
     MOVE    LST-F24           TO   CSV6-M18.
     MOVE    ","               TO   CSV6-MK18.
*完納担当者ＣＤ
     IF      LST-F24  =  "1"
             MOVE    LST-F26   TO   CSV6-M19(1:4)
             MOVE    "_"       TO   CSV6-M19(5:1)
             MOVE    LST-F27   TO   CSV6-M19(6:2)
     END-IF.
     MOVE    ","               TO   CSV6-MK19.
*完納担当者名
*    担当者名取得
     IF      LST-F24  =  "1"
             MOVE LST-F26              TO   TAN-F01
             MOVE LST-F27              TO   TAN-F02
             READ TANMS1
                INVALID
                  MOVE  ALL NC"＊"     TO   TAN-F03
             END-READ
             MOVE    X"28"             TO   CSV6-MS20
             MOVE    TAN-F03           TO   CSV6-M20
             MOVE    X"29"             TO   CSV6-ME20
     END-IF.
     MOVE    ","               TO   CSV6-MK20.
*入力区分
     IF      LST-MF91  =    "1"
             MOVE     "E"      TO   CSV6-M21
     ELSE
             MOVE     " "      TO   CSV6-M21
     END-IF.
     MOVE    ","               TO   CSV6-MK21.
*発注／入荷区分
     MOVE    X"28"             TO   CSV6-MS22.
     IF      LST-MF11  =    "1"
             MOVE    NC"発注"  TO   CSV6-M22
     ELSE
             MOVE    NC"入荷"  TO   CSV6-M22
     END-IF.
     MOVE    X"29"             TO   CSV6-ME22.
     MOVE    ","               TO   CSV6-MK22.
*発注日
     IF      LST-MF11  =    "1"
             MOVE    LST-MF12(1:4)  TO   CSV6-M23(1:4)
             MOVE    "/"            TO   CSV6-M23(5:1)
             MOVE    LST-MF12(5:2)  TO   CSV6-M23(6:2)
             MOVE    "/"            TO   CSV6-M23(8:1)
             MOVE    LST-MF12(7:2)  TO   CSV6-M23(9:2)
     ELSE
             MOVE    SPACE          TO   CSV6-M23
     END-IF.
     MOVE    ","                    TO   CSV6-MK23.
*入荷日
     IF      LST-MF11  =    "2"
             MOVE    LST-MF12(1:4)  TO   CSV6-M24(1:4)
             MOVE    "/"            TO   CSV6-M24(5:1)
             MOVE    LST-MF12(5:2)  TO   CSV6-M24(6:2)
             MOVE    "/"            TO   CSV6-M24(8:1)
             MOVE    LST-MF12(7:2)  TO   CSV6-M24(9:2)
     ELSE
             MOVE    SPACE          TO   CSV6-M24
     END-IF.
     MOVE    ","                    TO   CSV6-MK24.
*棚番
     MOVE    LST-MF13               TO   CSV6-M25.
     MOVE    ","                    TO   CSV6-MK25.
*発注数_符号
     IF      LST-MF11  =    "1"
             IF      LST-MF14  <    0
                     MOVE     "-"   TO   CSV6-M261
             ELSE
                     MOVE     " "   TO   CSV6-M261
             END-IF
     END-IF.
*発注数_数量
     IF      LST-MF11  =    "1"
             MOVE    LST-MF14       TO   CSV6-M262
     END-IF.
     MOVE    ","                    TO   CSV6-MK26.
*入荷数_符号
     IF      LST-MF11  =    "2"
             IF      LST-F14   <    0
                     MOVE     "-"   TO   CSV6-M271
             ELSE
                     MOVE     " "   TO   CSV6-M271
             END-IF
     END-IF.
*入荷数_数量
     IF      LST-MF11  =    "2"
             MOVE    LST-MF14       TO   CSV6-M272
     END-IF.
     MOVE    ","                    TO   CSV6-MK27.
*ＭＳＧ
     MOVE    X"28"                  TO   CSV6-MS28.
     MOVE    LST-MF15               TO   CSV6-M28.
     MOVE    X"29"                  TO   CSV6-ME28.
     MOVE    ","                    TO   CSV6-MK28.
*
 CSV6-SET-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
     MOVE     OUT-CNT    TO      MSG-OUT01.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
*
     CLOSE     ZSOKMS1
               TANMS1
               SFRLSTW1
               SFRCHK.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
