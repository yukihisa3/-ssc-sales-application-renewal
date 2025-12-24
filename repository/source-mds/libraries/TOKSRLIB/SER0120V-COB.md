# SER0120V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SER0120V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　システム共通サブシステム　　　　　*
*    モジュール名　　　　：　Ｄ３６５エラーリカバリリスト　　　*
*    　　　　　　　　　　　　ＣＳＶ出力　　　　　　　　　　　　*
*    作成日／作成者　　　：　2021/12/07 INOUE                  *
*    処理概要　　　　　　：　パラメタにに合致するレコードより　*
*                            リスト（ＣＳＶ）出力する。　　　　*
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SER0120V.
*                  流用:SSY3942V.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/12/07.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<Ｄ３６５累積ファイル>>********************************
     SELECT  ERRRUIL5  ASSIGN    TO        DA-01-VI-ERRRUIL5
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       ERR-F01
                                           ERR-F041
                                           ERR-F042
                                           ERR-F043
                                           ERR-F044
                                           ERR-F045
                                           ERR-F046
                       FILE      STATUS    ERR-STATUS.
****<<担当者マスタ　　　　　 >>*********************************
     SELECT   TANMS1   ASSIGN    TO        DA-01-VI-TANMS1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TAN-F01  TAN-F02
                       FILE      STATUS    TAN-STATUS.
*
****<<リカバリリストＣＳＶ　 >>*********************************
     SELECT   ERRLSTF   ASSIGN    TO        DA-01-ERRLSTF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*Ｄ３６５累積ファイル
******************************************************************
 FD  ERRRUIL5           LABEL RECORD   IS   STANDARD.
     COPY     ERRRUIL5   OF        XFDLIB
              JOINING   ERR  AS   PREFIX.
*
******************************************************************
*担当者マスタ
******************************************************************
 FD  TANMS1             LABEL RECORD   IS   STANDARD.
     COPY     TANMS1     OF        XFDLIB
              JOINING   TAN  AS   PREFIX.
*
******************************************************************
*リカバリリストＣＳＶ
******************************************************************
 FD  ERRLSTF            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  1       RECORDS.
     COPY     HACXXX1   OF        XFDLIB
              JOINING   CSV       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶマルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     ERRLST1   OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトル行
     COPY     ERRLST2   OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.明細行
     COPY     ERRLST3   OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  ERR-STATUS          PIC  X(02).
     03  TAN-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  TANMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
*
*数字編集用
 01  WK-TANKA-X.
     03  WK-TANKA            PIC  9(07).99.
 01  WK-RITU-X.
     03  WK-RITU             PIC  9(03).9.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
 01  SYSTEM-TIME.
     03  SYS-HH                   PIC  9(02).
     03  SYS-MN                   PIC  9(02).
     03  SYS-SS                   PIC  9(02).
*
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SER0120V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SER0120V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SER0120V".
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
                               "累積データ読込件数　＝".
         05  MSG-IN01        PIC  ZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                               "ＣＳＶデータ作成件数＝".
         05  MSG-OUT01       PIC  ZZZ,ZZ9.
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
 01  LINK-IN-BUMCD          PIC   X(04).
 01  LINK-IN-TANCD          PIC   X(02).
 01  LINK-IN-ERR1           PIC   X(01).
 01  LINK-IN-ERR2           PIC   X(01).
 01  LINK-IN-ERR3           PIC   X(01).
 01  LINK-IN-ERR4           PIC   X(01).
 01  LINK-IN-KANRINO        PIC   9(08).
 01  LINK-IN-BDATE          PIC   9(08).
 01  LINK-IN-BTIME          PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE     DIVISION  USING
                                     LINK-IN-BUMCD
                                     LINK-IN-TANCD
                                     LINK-IN-ERR1
                                     LINK-IN-ERR2
                                     LINK-IN-ERR3
                                     LINK-IN-ERR4
                                     LINK-IN-KANRINO
                                     LINK-IN-BDATE
                                     LINK-IN-BTIME.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ERRRUIL5.
     MOVE      "ERRRUIL5"   TO   AB-FILE.
     MOVE      ERR-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ERRLSTF.
     MOVE      "ERRLSTF"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TANMS1.
     MOVE      "TANMS1 "   TO   AB-FILE.
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
*ＣＳＶ出力
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

     DISPLAY  MSG-START UPON CONS.

     OPEN     INPUT     ERRRUIL5  TANMS1.
     OPEN     OUTPUT    ERRLSTF.
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
     MOVE     "20"       TO   SYS-DATEW(1:2).
     MOVE     SYS-DATE   TO   SYS-DATEW(3:6).
     ACCEPT   SYSTEM-TIME       FROM      TIME.
*
     MOVE  SPACE                TO   ERR-REC.
     INITIALIZE                      ERR-REC.
     MOVE   LINK-IN-KANRINO     TO   ERR-F01.
*D365エラー累積ファイルSTART
     START  ERRRUIL5  KEY  >=   ERR-F01  ERR-F041 ERR-F042
                                ERR-F043 ERR-F044 ERR-F045
            INVALID   KEY
               MOVE     "END"      TO   END-FLG
               MOVE     4010       TO   PROGRAM-STATUS
               DISPLAY NC"＃対象データ無し１＃" UPON CONS
               GO                  TO   INIT-EXIT
     END-START.
*
*D365エラー累積ファイル読込み
     PERFORM ERRRUIL5-RD-SEC.
     IF    END-FLG   =   "END"
           MOVE     4010       TO   PROGRAM-STATUS
           DISPLAY NC"＃対象データ無し２＃" UPON CONS
           GO                  TO   INIT-EXIT
     END-IF.
*
*終了判定
*  ＣＳＶ　　編集・出力　　 1.帳票タイトル行
     PERFORM  ERRLST1-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*  ＣＳＶ　　編集・出力　　 2.項目タイトル行
     PERFORM  ERRLST2-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*  ＣＳＶ　　初期値設定　　 3.明細
     PERFORM  ERRLST3-INIT-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*    エラー累積データ読み込み　　　
****************************************************************
 ERRRUIL5-RD-SEC            SECTION.
*
     MOVE    "ERRRUIL5-RD-SEC"    TO   S-NAME.
*
     READ     ERRRUIL5
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    ERRRUIL5-RD-EXIT
     END-READ.
*
 ERRRUIL5-RD-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"     TO      S-NAME.
*
 MAIN-01.
*対象チェック
*　　管理番号
     IF       ERR-F01   NOT =  LINK-IN-KANRINO
              MOVE     "END"      TO   END-FLG
              GO                  TO   MAIN-EXIT
     END-IF.
 MAIN-011.
*　　リカバリ対象（1～4全" "）
     IF     ( LINK-IN-ERR1 = " " ) AND
            ( LINK-IN-ERR2 = " " ) AND
            ( LINK-IN-ERR3 = " " ) AND
            ( LINK-IN-ERR4 = " " )
              GO                  TO   MAIN-02
     END-IF.
 MAIN-012.
*　　リカバリ対象1
     IF       LINK-IN-ERR1    = "1"
              IF (  ERR-F043  = "01" ) OR
                 (  ERR-F043  = "02" )
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-013.
*　　リカバリ対象2
     IF       LINK-IN-ERR2    = "1"
              IF    ERR-F043  = "03"
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-014.
*　　リカバリ対象3
     IF       LINK-IN-ERR3    = "1"
              IF    ERR-F043  = "04"
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-015.
*　　リカバリ対象4
     IF       LINK-IN-ERR4    = "1"
              IF    ERR-F043  = "05"
                    GO                 TO   MAIN-02
              END-IF
     END-IF.
 MAIN-016.
*　　対象外　読み飛ばし
     GO                         TO   MAIN-99.
*
 MAIN-02.
     ADD        1               TO   RD-CNT.
*
*  発注一覧ＣＳＶ　　編集　　　 3.明細行
     PERFORM  ERRLST3-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV3-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
 MAIN-99.
*    エラー累積データ次レコード読込み
     PERFORM ERRRUIL5-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*発注一覧ＣＳＶ  SET   1.帳票タイトル行
****************************************************************
 ERRLST1-SET-SEC     SECTION.
*
     MOVE   "ERRLST1-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV1-REC.
*
     MOVE    X"28"                                TO CSV1-CS00.
     MOVE    NC"Ｄ３６５エラーリカバリリスト"     TO CSV1-C00.
     MOVE    X"29"                                TO CSV1-CE00.
     MOVE    ","                                  TO CSV1-CK00.
*
 CHGXXX11-SET-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   SET   2.帳票タイトル行
****************************************************************
 ERRLST2-SET-SEC     SECTION.
*
     MOVE   "ERRLST2-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"管理番号"                         TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"バッチ日付"                       TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"バッチ時刻"                       TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"ＰＣ処理日"                       TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"時刻"                             TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"エラー区分"                       TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
*    MOVE    X"28"                                TO CSV2-KS07.
*    MOVE    NC"　"                               TO CSV2-K07.
*    MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"伝票番号"                         TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"行"                               TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"枝番"                             TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"ＪＡＮＣＤ"                       TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"リカバリ実行担当者"               TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
*    MOVE    X"28"                                TO CSV2-KS13.
*    MOVE    NC"　"                               TO CSV2-K13.
*    MOVE    X"29"                                TO CSV2-KE13.
     MOVE    ","                                  TO CSV2-KK13.
*
     MOVE    X"28"                                TO CSV2-KS14.
     MOVE    NC"実行日"                           TO CSV2-K14.
     MOVE    X"29"                                TO CSV2-KE14.
     MOVE    ","                                  TO CSV2-KK14.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"時刻"                             TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
 ERRLST2-SET-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   初期値設定   3.明細行
****************************************************************
 ERRLST3-INIT-SEC    SECTION.
*
     MOVE     "ERRLST3-INIT-SEC" TO  S-NAME.
*
     MOVE     SPACE          TO   CSV3-REC.
*制御バイト
     MOVE    X"28"           TO   CSV3-H071
                                  CSV3-H131.
     MOVE    X"29"           TO   CSV3-H072
                                  CSV3-H132.
*カンマ
     MOVE    ","             TO   CSV3-C01
                                  CSV3-C02
                                  CSV3-C03
                                  CSV3-C04
                                  CSV3-C05
                                  CSV3-C06
                                  CSV3-C07
                                  CSV3-C08
                                  CSV3-C09
                                  CSV3-C10
                                  CSV3-C11
                                  CSV3-C12
                                  CSV3-C13
                                  CSV3-C14
                                  CSV3-C15.
*
 ERRLST3-INIT-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   SET   3.明細行
****************************************************************
 ERRLST3-SET-SEC     SECTION.
*
     MOVE     "ERRLST3-SET-SEC"  TO  S-NAME.
*
*管理番号
     MOVE    LINK-IN-KANRINO TO   CSV3-H01.
*バッチ日付
     MOVE    LINK-IN-BDATE   TO   CSV3-H02.
*バッチ時刻
     MOVE    LINK-IN-BTIME   TO   CSV3-H03.
*ＰＣ処理日
     MOVE    ERR-F041        TO   CSV3-H04.
*ＰＣ処理時刻
     MOVE    ERR-F042        TO   CSV3-H05.
*エラー区分（区分）
     MOVE    ERR-F043        TO   CSV3-H06.
*エラー区分（名称）
     EVALUATE ERR-F043
        WHEN  "01"
              MOVE NC"受注情報"     TO    CSV3-H07
        WHEN  "02"
              MOVE NC"売上情報"     TO    CSV3-H07
        WHEN  "03"
              MOVE NC"入荷実績情報" TO    CSV3-H07
        WHEN  "04"
              MOVE NC"移動実績情報" TO    CSV3-H07
        WHEN  "05"
              MOVE NC"作業依頼情報" TO    CSV3-H07
        WHEN  OTHER
              MOVE ALL NC"＊"       TO    CSV3-H07
     END-EVALUATE.
*伝票番号
*    MOVE    ERR-F044        TO   CSV3-H08.
     MOVE    SPACE           TO   CSV3-H08.
     EVALUATE ERR-F043
        WHEN  "01"
              MOVE   ERR-F044(3:18)   TO    CSV3-H08
        WHEN  "02"
              MOVE   ERR-F044(3:18)   TO    CSV3-H08
        WHEN  "03"
              MOVE   ERR-F044(14:7)   TO    CSV3-H08
        WHEN  "04"
              MOVE   ERR-F044(14:7)   TO    CSV3-H08
        WHEN  "05"
              MOVE   ERR-F044(14:7)   TO    CSV3-H08
        WHEN  OTHER
              MOVE   ERR-F044         TO    CSV3-H08
     END-EVALUATE.
*行
     MOVE    ERR-F045        TO   CSV3-H09.
*枝番
     MOVE    ERR-F046        TO   CSV3-H10.
*ＪＡＮＣＤ
     MOVE    ERR-F047        TO   CSV3-H11.
*実行担当者（コード）
     MOVE    ERR-F97         TO   CSV3-H12.
*実行担当者（名称）
     MOVE    LINK-IN-BUMCD   TO   TAN-F01.
     MOVE    ERR-F97         TO   TAN-F02.
     PERFORM  TANMS1-READ-SEC.
     IF   TANMS1-INV-FLG = "INV"
          MOVE  ALL NC"＊"     TO        CSV3-H13
     ELSE
          MOVE  TAN-F03        TO        CSV3-H13
     END-IF.
*実行日
     MOVE    ERR-F98         TO   CSV3-H14.
*時刻
     MOVE    ERR-F99         TO   CSV3-H15.
*
 ERRLST3-SET-EXIT.
     EXIT.
***************************************************************
*             担当者マスタ読込
***************************************************************
 TANMS1-READ-SEC       SECTION.
*
     MOVE    "TANMS1-READ-SEC" TO        S-NAME.
*
     READ     TANMS1
              INVALID      MOVE  "INV"    TO   TANMS1-INV-FLG
              NOT  INVALID MOVE  SPACE    TO   TANMS1-INV-FLG
     END-READ.
*
 TANMS1-READ-EXIT.
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
     CLOSE    ERRRUIL5   TANMS1  ERRLSTF.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
