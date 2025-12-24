****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*                                                              *
*    業務名　　　　　　　：　社内振替　　　　　　　　　　　　　*
*    モジュール名　　　　：　社内振替発注残リスト　　　　　　  *
*    　　　　　　　　　　　　ＣＳＶデータ出力　　　　　　　　  *
*    作成日／作成者　　　：　2017/01/20 INOUE                  *
*    処理概要　　　　　　：　振分リスト出力ワークより、　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    流用元　　　　　　　：　SFU3100V                          *
*                                                              *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU3130V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/01/20.
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
     SELECT   SFRLSTW2  ASSIGN    TO        DA-01-VI-SFRLSTW2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       LST-F04 LST-F01
                                            LST-F02 LST-F03
                                            LST-F14 LST-F05
                                            LST-F06 LST-F07
                                            LST-F08
                        FILE      STATUS    LST-STATUS.
*振分リストＣＳＶ
     SELECT   SFRZAN    ASSIGN    TO        DA-01-S-SFRZAN
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*倉庫マスタ
     SELECT   ZSOKMS1   ASSIGN     TO      DA-01-VI-ZSOKMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     SOK-F01
                        FILE    STATUS     SOK-STATUS.
*仕入先マスタ
     SELECT   ZSHIMS1   ASSIGN     TO      DA-01-VI-ZSHIMS1
                        ORGANIZATION       INDEXED
                        ACCESS     MODE    RANDOM
                        RECORD     KEY     SHI-F01
                        FILE    STATUS     SHI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*振分リストワーク
******************************************************************
 FD  SFRLSTW2           LABEL RECORD   IS   STANDARD.
     COPY     SFRLSTW2  OF        XFDLIB
              JOINING   LST  AS   PREFIX.
*
******************************************************************
*振分リストＣＳＶ
******************************************************************
 FD  SFRZAN            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS   13        RECORDS.
     COPY     SFRZAN01   OF        XFDLIB
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
*仕入先マスタ
******************************************************************
 FD  ZSHIMS1
                        LABEL RECORD   IS   STANDARD.
     COPY     ZSHIMS1   OF        XFDLIB
              JOINING   SHI  AS   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*振分リストＣＳＶマルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     SFRZAN01  OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトル行
     COPY     SFRZAN02  OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.明細行
     COPY     SFRZAN03  OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  LST-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  SOK-STATUS          PIC  X(02).
     03  SHI-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SFRLSTW2-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  SOKO-HIT-FLG            PIC  X(03)     VALUE  SPACE.
 01  KEY-BRK-FLG             PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*計算
 01  ZAN                     PIC S9(09).
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
         05  ST-PG           PIC  X(08)  VALUE "SFU3130V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SFU3130V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SFU3130V".
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
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING LINK-IN-OUTSOKO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRLSTW2.
     MOVE      "SFRLSTW2"   TO   AB-FILE.
     MOVE      LST-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SFRZAN.
     MOVE      "SFRZAN"     TO   AB-FILE.
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
                        PROCEDURE   ZSHIMS1.
     MOVE      "ZSHIMS1"    TO   AB-FILE.
     MOVE      SHI-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SFRLSTW2  ZSOKMS1  ZSHIMS1.
     OPEN     OUTPUT    SFRZAN.
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
     PERFORM SFRLSTW2-READ-SEC.
*終了判定
     IF   SFRLSTW2-END-FLG  =  "END"
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
 SFRLSTW2-READ-SEC    SECTION.
*
     MOVE    "SFRLSTW2-READ-SEC"    TO   S-NAME.
*
     READ     SFRLSTW2
              AT  END
                  MOVE     "END"    TO  SFRLSTW2-END-FLG
                  GO                TO  SFRLSTW2-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
 SFRLSTW2-READ-EXIT.
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
*****INITIALIZE                      SUM-AREA.
*****INITIALIZE                      CSV-REC.
*
*  振分ＣＳＶ SET/OUT  1.帳票タイトル行
     PERFORM  CSV1-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  振分ＣＳＶ SET/OUT  2.項目タイトル行
     PERFORM  CSV2-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
 MAIN-02.
*
*  振分ＣＳＶ SET/OUT  3.明細行
     PERFORM  CSV3-SET-SEC.
*
*    レコード出力
     WRITE    CSV-REC            FROM  CSV3-REC.
     ADD      1                  TO    OUT-CNT.
     MOVE     SPACE              TO    CSV3-REC.
*
*    振分リストワーク読込み
     PERFORM  SFRLSTW2-READ-SEC.
     IF       SFRLSTW2-END-FLG    =    "END"
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
     MOVE    NC"社内振替発注残リストＣＳＶ"       TO CSV1-C00.
     MOVE    X"29"                                TO CSV1-CE00.
     MOVE    ","                                  TO CSV1-CK00.
*
 CSV1-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   2.項目タイトル行
****************************************************************
 CSV2-SET-SEC     SECTION.
*
     MOVE   "CSV2-SET-SEC"  TO  S-NAME.
*
 CSV2-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"倉庫ＣＤ"                         TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"倉庫名"                           TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"伝票区分"                         TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"年度"                             TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"シーズン"                         TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"入荷予定日"                       TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"サカタ商品ＣＤ"                   TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"商品名１"                         TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"商品名２"                         TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"仕入先ＣＤ"                       TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"仕入先名"                         TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"棚番"                             TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
     MOVE    X"28"                                TO CSV2-KS13.
     MOVE    NC"発注数合計"                       TO CSV2-K13.
     MOVE    X"29"                                TO CSV2-KE13.
     MOVE    ","                                  TO CSV2-KK13.
*
     MOVE    X"28"                                TO CSV2-KS14.
     MOVE    NC"入荷数合計"                       TO CSV2-K14.
     MOVE    X"29"                                TO CSV2-KE14.
     MOVE    ","                                  TO CSV2-KK14.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"発注残"                           TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS16.
     MOVE    NC"ＭＳＧ"                           TO CSV2-K16.
     MOVE    X"29"                                TO CSV2-KE16.
     MOVE    ","                                  TO CSV2-KK16.
*
 CSV2-SET-EXIT.
     EXIT.
****************************************************************
*振分ＣＳＶSET   3.明細行
****************************************************************
 CSV3-SET-SEC     SECTION.
*
     MOVE     "CSV3-SET-SEC"  TO  S-NAME.
*
 CSV3-SET-01.
*
*倉庫ＣＤ
     MOVE    LINK-IN-OUTSOKO  TO   CSV3-M01.
     MOVE    ","              TO   CSV3-MK01.
*倉庫名
     MOVE    X"28"            TO   CSV3-MS02.
     MOVE    SOK-F02          TO   CSV3-M02.
     MOVE    X"29"            TO   CSV3-ME02.
     MOVE    ","              TO   CSV3-MK02.
*伝票区分
     MOVE    LST-F01          TO   CSV3-M03.
     MOVE    ","              TO   CSV3-MK03.
*年度
     MOVE    LST-F02          TO   CSV3-M04.
     MOVE    ","              TO   CSV3-MK04.
*シーズン
     MOVE    LST-F03          TO   CSV3-M05.
     MOVE    ","              TO   CSV3-MK05.
*入荷予定日
     MOVE    LST-F14          TO   CSV3-M06.
     MOVE    ","              TO   CSV3-MK06.
*サカタ商品ＣＤ
     MOVE    LST-F05          TO   CSV3-M07.
     MOVE    ","              TO   CSV3-MK07.
*商品名１
     MOVE    X"28"            TO   CSV3-MS08.
     MOVE    LST-F09          TO   CSV3-M08.
     MOVE    X"29"            TO   CSV3-ME08.
     MOVE    ","              TO   CSV3-MK08.
*商品名２
     MOVE    X"28"            TO   CSV3-MS09.
     MOVE    LST-F10          TO   CSV3-M09.
     MOVE    X"29"            TO   CSV3-ME09.
     MOVE    ","              TO   CSV3-MK09.
*仕入先ＣＤ
     MOVE    LST-F12          TO   CSV3-M10.
     MOVE    ","              TO   CSV3-MK10.
*仕入先名
     MOVE LST-F12             TO   SHI-F01.
     READ ZSHIMS1
        INVALID
          MOVE   SPACE        TO   SHI-F02
     END-READ.
     MOVE    X"28"            TO   CSV3-MS11.
     MOVE    SHI-F02          TO   CSV3-M11.
     MOVE    X"29"            TO   CSV3-ME11.
     MOVE    ","              TO   CSV3-MK11.
*棚番
     MOVE    LST-F11          TO   CSV3-M12.
     MOVE    ","              TO   CSV3-MK12.
*発注数合計_符号
     IF      LST-F15   <    0
             MOVE     "-"     TO   CSV3-M131
     ELSE
             MOVE     " "     TO   CSV3-M131
     END-IF.
*発注数合計_数量
     MOVE    LST-F15          TO   CSV3-M132.
     MOVE    ","              TO   CSV3-MK13.
*入荷数合計_符号
     IF      LST-F19   <    0
             MOVE     "-"     TO   CSV3-M141
     ELSE
             MOVE     " "     TO   CSV3-M141
     END-IF.
*入荷数合計_数量
     MOVE    LST-F19           TO   CSV3-M142.
     MOVE    ","               TO   CSV3-MK14.
*発注残_符号
     COMPUTE ZAN   =    LST-F15 - LST-F19.
     IF      ZAN   <    0
             MOVE     "-"      TO   CSV3-M151
     ELSE
             MOVE     " "      TO   CSV3-M151
     END-IF.
*発注残_数量
     MOVE    ZAN               TO   CSV3-M152.
     MOVE    ","               TO   CSV3-MK15.
*ＭＳＧ
     MOVE    X"28"             TO   CSV3-MS16.
     MOVE    LST-F23           TO   CSV3-M16.
     MOVE    X"29"             TO   CSV3-ME16.
     MOVE    ","               TO   CSV3-MK16.
*
 CSV3-SET-EXIT.
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
               ZSHIMS1
               SFRLSTW2
               SFRZAN.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
