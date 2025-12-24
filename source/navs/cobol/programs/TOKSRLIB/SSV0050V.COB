****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ジョイフル本田　　　　　　　　　　*
*    業務名　　　　　　　：　支払　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細ＣＳＶデータ作成　　　　　*
*    作成日／作成者　　　：　2019/03/07 INOUE                  *
*    処理概要　　　　　　：　支払データより　　　　　　　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSV0050V.
*流用元                 SSI7104L.TOKSLIBS
*                       SSV0030V.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/03/07.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払　合計データ >>--*
     SELECT   SITGG711  ASSIGN         DA-01-VI-SITGG711
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SGG-F01
                        STATUS         SGG-STATUS.
*----<< 支払　明細データ >>--*
     SELECT   SITGK711  ASSIGN         DA-01-VI-SITGK711
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SGK-F01  SGK-F02
                                       SGK-F04  SGK-F05
                                       SGK-F06
                        STATUS         SGK-STATUS.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-STATUS.
*----<< ＣＳＶ　　 >>--*
     SELECT   CSVGK71   ASSIGN         DA-01-S-CSVGK71
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         CSV-STATUS.
************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*----<< 支払　合計ファイル >>--*
 FD  SITGG711           LABEL RECORD   IS   STANDARD.
     COPY     SITGG711  OF        XFDLIB
              JOINING   SGG       PREFIX.
*----<< 支払　明細データ >>--*
 FD  SITGK711           LABEL RECORD   IS   STANDARD.
     COPY     SITGK711  OF        XFDLIB
              JOINING   SGK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< ＣＳＶ　　 >>--*
 FD  CSVGK71            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  20       RECORDS.
     COPY     CSVGK711  OF        XFDLIB
              JOINING   CSV       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶマルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     CSVGK711   OF        XFDLIB
              JOINING    CSV1 AS   PREFIX.
* 2.ヘッダタイトル行
     COPY     CSVGK712   OF        XFDLIB
              JOINING    CSV2 AS   PREFIX.
* 3.ヘッダ行
     COPY     CSVGK713   OF        XFDLIB
              JOINING    CSV3 AS   PREFIX.
* 4.明細行タイトル行
     COPY     CSVGK714   OF        XFDLIB
              JOINING    CSV4 AS   PREFIX.
* 5.明細行
     COPY     CSVGK715   OF        XFDLIB
              JOINING    CSV5 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  SGG-STATUS          PIC  X(02).
     03  SGK-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SITGG711-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  SITGK711-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TENPO-HIT-FLG           PIC  X(03)     VALUE  SPACE.
 01  KEY-BRK-FLG             PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*
*数量編集用
 01  WK-SGK-F07              PIC S9(08)     VALUE  ZERO.
*08桁用
 01  WK-HEN08                PIC  9(08).
 01  WK-HEN081.
     03  WK-HEN081-1         PIC  X(01).
     03  WK-HEN081-2         PIC  X(08).
*09桁用
 01  WK-HEN09                PIC  9(09).
 01  WK-HEN091.
     03  WK-HEN091-1         PIC  X(01).
     03  WK-HEN091-2         PIC  X(09).
*10桁用
 01  WK-HEN10                PIC  9(10).
 01  WK-HEN101.
     03  WK-HEN101-1         PIC  X(01).
     03  WK-HEN101-2         PIC  X(10).
*11桁用
 01  WK-HEN11                PIC  9(11).
 01  WK-HEN111.
     03  WK-HEN111-1         PIC  X(01).
     03  WK-HEN111-2         PIC  X(11).
*12桁用
 01  WK-HEN12                PIC  9(12).
 01  WK-HEN121.
     03  WK-HEN121-1         PIC  X(01).
     03  WK-HEN121-2         PIC  X(12).
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
         05  ST-PG           PIC  X(08)  VALUE "SSV0050V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSV0050V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSV0050V".
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
                             " 元データ読込件数   = ".
         05  MSG-IN01        PIC  ZZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
     03  MSG-OUT.
         05  FILLER          PIC  X(02)  VALUE "##".
         05  FILLER          PIC  X(24)  VALUE
                             " CSVデータ抽出件数  = ".
         05  MSG-OUT01       PIC  ZZZZ,ZZ9.
         05  FILLER          PIC  X(06)  VALUE
                             " 件 ".
         05  FILLER          PIC  X(01)  VALUE "#".
*    03  MSG-IN2.
*        05  FILLER          PIC  X(02)  VALUE "##".
*        05  FILLER          PIC  X(24)  VALUE
*                            " 明細情報読込件数   = ".
*        05  MSG-IN02        PIC  ZZ,ZZ9.
*        05  FILLER          PIC  X(06)  VALUE
*                            " 件 ".
*        05  FILLER          PIC  X(01)  VALUE "#".
*    03  MSG-OUT2.
*        05  FILLER          PIC  X(02)  VALUE "##".
*        05  FILLER          PIC  X(24)  VALUE
*                            " CSVデータ抽出件数  = ".
*        05  MSG-OUT02       PIC  ZZ,ZZ9.
*        05  FILLER          PIC  X(06)  VALUE
*                            " 件 ".
*        05  FILLER          PIC  X(01)  VALUE "#".
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
*LINKAGE                SECTION.
*01  LINK-IN-SIMEBI          PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITGG711.
     MOVE      "SITGG711"   TO   AB-FILE.
     MOVE      SGG-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITGK711.
     MOVE      "SITGK711"   TO   AB-FILE.
     MOVE      SGK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CSVGK71.
     MOVE      "CSVGK71"    TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TENMS1.
     MOVE      "TENMS1 "    TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
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
     PERFORM  MAIN1-SEC
              UNTIL     END-FLG   =  "END".
     MOVE     SPACE     TO        END-FLG.
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
     OPEN     INPUT     SITGG711  SITGK711  TENMS1.
     OPEN     OUTPUT    CSVGK71.
     DISPLAY  MSG-START UPON CONS.
*    DISPLAY "SIMEBI = " LINK-IN-SIMEBI  UPON CONS.
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
*INPUT2読込
     PERFORM  SITGK711-READ-SEC.
     IF       SITGK711-END-FLG  =  "END"
              DISPLAY  NC"＃＃出力対象データ無し＃＃" UPON CONS
              MOVE    "END"     TO    END-FLG
              MOVE     4010     TO    PROGRAM-STATUS
              GO                TO    INIT-EXIT
     END-IF.
*
*INPUT1読込
     MOVE     SGK-F01           TO   SGG-F01.
     PERFORM  SITGG711-READ-SEC.
     IF       SITGG711-END-FLG  =  "END"
              DISPLAY  NC"＃＃出力対象無し＃＃" UPON CONS
              MOVE    "END"     TO   END-FLG
              MOVE     4010     TO   PROGRAM-STATUS
              GO                TO   INIT-EXIT
     END-IF.
 INIT-EXIT.
     EXIT.
****************************************************************
*　　INPUT2読込
****************************************************************
 SITGK711-READ-SEC    SECTION.
*
     MOVE    "SITGK711-READ-SEC"    TO   S-NAME.
*
     READ     SITGK711
              AT  END
                  MOVE     "END"    TO  SITGK711-END-FLG
                  GO                TO  SITGK711-READ-EXIT
     END-READ.
*
*支払区分のチェック
     IF       SGK-F09  =  "40"
              GO           TO   SITGK711-READ-SEC
     END-IF.
*
     ADD      1     TO     RD-CNT.
*
     IF   RD-CNT(6:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 SITGK711-READ-EXIT.
     EXIT.
****************************************************************
*　　INPUT1読込
****************************************************************
 SITGG711-READ-SEC    SECTION.
*
     MOVE    "SITGG711-READ-SEC"    TO   S-NAME.
*
     READ     SITGG711
       INVALID
              MOVE    "END"    TO   SITGG711-END-FLG
              GO               TO   SITGG711-READ-EXIT
     END-READ.
*
 SITGG711-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
 MAIN1-01.
     MOVE     SPACE          TO      CSV-REC.
     MOVE     SPACE          TO      CSV1-REC.
     MOVE     SPACE          TO      CSV2-REC.
     MOVE     SPACE          TO      CSV3-REC.
     MOVE     SPACE          TO      CSV4-REC.
     MOVE     SPACE          TO      CSV5-REC.
*
*  ＣＳＶ SET/OUT  1.帳票タイトル行
     PERFORM  CSVGK711-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
     MOVE     SPACE     TO        CSV1-REC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  ＣＳＶ SET/OUT  2.ヘッダタイトル行 (上行)
     PERFORM  CSVGK7121-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
*  ＣＳＶ SET/OUT  2.ヘッダタイトル行 (下行)
     PERFORM  CSVGK7122-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
*  ＣＳＶ SET/OUT  3.ヘッダ行
     PERFORM  CSVGK713-SET-SEC.
     WRITE    CSV-REC   FROM      CSV3-REC.
     MOVE     SPACE     TO        CSV3-REC.
     WRITE    CSV-REC   FROM      CSV3-REC.
*
*  ＣＳＶ SET/OUT  4.明細タイトル行
     PERFORM  CSVGK714-SET-SEC.
     WRITE    CSV-REC   FROM      CSV4-REC.
*

 MAIN1-02.
*
*  ＣＳＶ SET/OUT  5.明細行
     PERFORM  CSVGK715-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV5-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    INPUT2 読込み
     PERFORM  SITGK711-READ-SEC.
     IF       SITGK711-END-FLG    =    "END"
              MOVE    "END"      TO    END-FLG
              GO                 TO    MAIN1-EXIT
     END-IF.
*
     GO       TO       MAIN1-02.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ  SET   1.帳票タイトル行
****************************************************************
 CSVGK711-SET-SEC     SECTION.
*
     MOVE   "CSVGK711-SET-SEC"  TO  S-NAME.
*
 CSVGK711-SET-01.
*
     MOVE    SPACE                              TO CSV1-REC.
*
     MOVE    X"28"                              TO CSV1-CS00.
     MOVE    NC"【ジョイフル本田支払明細】"     TO CSV1-C00.
     MOVE    X"29"                              TO CSV1-CE00.
     MOVE    ",,"                               TO CSV1-CK00.
*
*
     MOVE    X"28"                              TO CSV1-CS01.
     MOVE    NC"処理日："                       TO CSV1-C01.
     MOVE    X"29"                              TO CSV1-CE01.
     MOVE    ","                                TO CSV1-CK01.
*
     MOVE    SYS-DATEW(1:4)                     TO CSV1-C02(1:4).
     MOVE    "/"                                TO CSV1-C02(5:1).
     MOVE    SYS-DATEW(5:2)                     TO CSV1-C02(6:2).
     MOVE    "/"                                TO CSV1-C02(8:1).
     MOVE    SYS-DATEW(7:2)                     TO CSV1-C02(9:2).
     MOVE    ","                                TO CSV1-CK02.
*
*    MOVE    X"28"                              TO CSV1-CS03.
*    MOVE    NC"締切日："                       TO CSV1-C03.
*    MOVE    X"29"                              TO CSV1-CE03.
*    MOVE    ","                                TO CSV1-CK03.
*
*    MOVE    XXX-F0X(1:4)                       TO CSV1-C04(1:4).
*    MOVE    "/"                                TO CSV1-C04(5:1).
*    MOVE    XXX-F0X(5:2)                       TO CSV1-C04(6:2).
*    MOVE    "/"                                TO CSV1-C04(8:1).
*    MOVE    XXX-F0X(7:2)                       TO CSV1-C04(9:2).
*    MOVE    ","                                TO CSV1-CK04.
*
 CSVGK711-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   2.ヘッダタイトル行 (上行)
****************************************************************
 CSVGK7121-SET-SEC     SECTION.
*
     MOVE   "CSVGK7121-SET-SEC"  TO  S-NAME.
*
 CSVGK7121-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"＜支払情報＞"                     TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"　"                               TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"　"                               TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"　"                               TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"　"                               TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"　"                               TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"　"                               TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"　"                               TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"　"                               TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
 CSVGK7121-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   2.ヘッダタイトル行 (下行)
****************************************************************
 CSVGK7122-SET-SEC     SECTION.
*
     MOVE   "CSVGK7121-SET-SEC"  TO  S-NAME.
*
 CSVGK7122-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"取引先"                           TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"　"                               TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"締切日付"                         TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"支払伝票枚数"                     TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"訂正伝票枚数"                     TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"未検収伝票枚数"                   TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"支払金額"                         TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"消費税金額"                       TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"税込支払金額"                     TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
 CSVGK7122-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   3.ヘッダ行
****************************************************************
 CSVGK713-SET-SEC     SECTION.
*
     MOVE     "CSVGK713-SET-SEC"  TO  S-NAME.
*
 CSVGK713-SET-01.
*
     MOVE    SPACE     TO   CSV3-REC.
*
*制御バイト
     MOVE    X"28"     TO   CSV3-MS02.
     MOVE    X"29"     TO   CSV3-ME02.
*取引先CD
     MOVE    2243           TO  CSV3-M01.
     MOVE    ","            TO  CSV3-MK01.
*取引先名
     MOVE    NC"ジョイフル本田"       TO   CSV3-M02.
     MOVE    ","       TO   CSV3-MK02.
*締切日付
     MOVE    "20"           TO  CSV3-M03(1:2).
     MOVE    SGG-F01(1:2)   TO  CSV3-M03(3:2).
     MOVE    "/"            TO  CSV3-M03(5:1).
     MOVE    SGG-F01(3:2)   TO  CSV3-M03(6:2).
     MOVE    "/"            TO  CSV3-M03(8:1).
     MOVE    SGG-F01(5:2)   TO  CSV3-M03(9:2).
     MOVE    ","            TO  CSV3-MK03.
*支払伝票枚数
     MOVE    SGG-F02        TO  CSV3-M04.
     MOVE    ","            TO  CSV3-MK04.
*訂正伝票枚数
     MOVE    SGG-F03   TO   CSV3-M05.
     MOVE    ","       TO   CSV3-MK05.
*未検収伝票枚数
     MOVE    SGG-F04   TO   CSV3-M06.
     MOVE    ","       TO   CSV3-MK06.
*支払金額
     INITIALIZE             WK-HEN091.
     IF      SGG-F05  <  ZERO
             MOVE "-"  TO   WK-HEN091-1
     END-IF.
     MOVE    SGG-F05   TO   WK-HEN09.
     MOVE    WK-HEN09  TO   WK-HEN091-2.
     MOVE    WK-HEN091 TO   CSV3-M07.
     MOVE    ","       TO   CSV3-MK07.
*消費税金額
     INITIALIZE             WK-HEN091.
     IF      SGG-F06  <  ZERO
             MOVE "-"  TO   WK-HEN091-1
     END-IF.
     MOVE    SGG-F06   TO   WK-HEN09.
     MOVE    WK-HEN09  TO   WK-HEN091-2.
     MOVE    WK-HEN091 TO   CSV3-M08.
     MOVE    ","       TO   CSV3-MK08.
*税込支払金額
     INITIALIZE             WK-HEN091.
     IF      SGG-F07  <  ZERO
             MOVE "-"  TO   WK-HEN091-1
     END-IF.
     MOVE    SGG-F07   TO   WK-HEN09.
     MOVE    WK-HEN09  TO   WK-HEN091-2.
     MOVE    WK-HEN091 TO   CSV3-M09.
     MOVE    ","       TO   CSV3-MK09.
*
 CSVGK713-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   4.明細タイトル行
****************************************************************
 CSVGK714-SET-SEC     SECTION.
*
     MOVE   "CSVGK714-SET-SEC"  TO  S-NAME.
*
 CSVGK714-SET-01.
*
     MOVE    SPACE                                TO CSV4-REC.
*
     MOVE    X"28"                                TO CSV4-MTKS01.
     MOVE    NC"店舗"                             TO CSV4-MTK01.
     MOVE    X"29"                                TO CSV4-MTKE01.
     MOVE    ","                                  TO CSV4-MTKK01.
*
     MOVE    X"28"                                TO CSV4-MTKS02.
     MOVE    NC"　"                               TO CSV4-MTK02.
     MOVE    X"29"                                TO CSV4-MTKE02.
     MOVE    ","                                  TO CSV4-MTKK02.
*
     MOVE    X"28"                                TO CSV4-MTKS03.
     MOVE    NC"取引日付"                         TO CSV4-MTK03.
     MOVE    X"29"                                TO CSV4-MTKE03.
     MOVE    ","                                  TO CSV4-MTKK03.
*
     MOVE    X"28"                                TO CSV4-MTKS04.
     MOVE    NC"伝票区分"                         TO CSV4-MTK04.
     MOVE    X"29"                                TO CSV4-MTKE04.
     MOVE    ","                                  TO CSV4-MTKK04.
*
     MOVE    X"28"                                TO CSV4-MTKS05.
     MOVE    NC"　"                               TO CSV4-MTK05.
     MOVE    X"29"                                TO CSV4-MTKE05.
     MOVE    ","                                  TO CSV4-MTKK05.
*
     MOVE    X"28"                                TO CSV4-MTKS06.
     MOVE    NC"伝票番号"                         TO CSV4-MTK06.
     MOVE    X"29"                                TO CSV4-MTKE06.
     MOVE    ","                                  TO CSV4-MTKK06.
*
     MOVE    X"28"                                TO CSV4-MTKS07.
     MOVE    NC"支払金額"                         TO CSV4-MTK07.
     MOVE    X"29"                                TO CSV4-MTKE07.
     MOVE    ","                                  TO CSV4-MTKK07.
*
     MOVE    X"28"                                TO CSV4-MTKS08.
     MOVE    NC"備考"                             TO CSV4-MTK08.
     MOVE    X"29"                                TO CSV4-MTKE08.
     MOVE    ","                                  TO CSV4-MTKK08.
*
 CSVGK714-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   5.明細行
****************************************************************
 CSVGK715-SET-SEC     SECTION.
*
     MOVE     "CSVGK715-SET-SEC"  TO  S-NAME.
*
 CSVGK715-SET-01.
*
     MOVE    SPACE     TO   CSV5-REC.
*
*制御バイト
     MOVE    X"28"     TO   CSV5-MSMS02
                            CSV5-MSMS05
                            CSV5-MSMS08.
     MOVE    X"29"     TO   CSV5-MSME02
                            CSV5-MSME05
                            CSV5-MSME08.
*店舗ＣＤ
     MOVE    SGK-F02   TO   CSV5-MSM01.
     MOVE    ","       TO   CSV5-MSMK01.
*店舗名
     MOVE    2243      TO   TEN-F52.
     MOVE    SGK-F02   TO   TEN-F011.
     PERFORM TENMS1-READ-SEC.
     IF      TENMS1-INV-FLG = "INV"
             MOVE      ALL NC"＊"     TO   CSV5-MSM02
     ELSE
             MOVE      TEN-F03        TO   CSV5-MSM02
     END-IF.
     MOVE    ","       TO   CSV5-MSMK02.
*取引日付
     MOVE    "20"           TO  CSV5-MSM03(1:2).
     MOVE    SGK-F04(1:2)   TO  CSV5-MSM03(3:2).
     MOVE    "/"            TO  CSV5-MSM03(5:1).
     MOVE    SGK-F04(3:2)   TO  CSV5-MSM03(6:2).
     MOVE    "/"            TO  CSV5-MSM03(8:1).
     MOVE    SGK-F04(5:2)   TO  CSV5-MSM03(9:2).
     MOVE    ","            TO  CSV5-MSMK03.
*伝票区分・名称
     EVALUATE SGK-F05
         WHEN     "31"
         MOVE      31             TO   CSV5-MSM04
         MOVE   NC"仕入"          TO   CSV5-MSM05
         WHEN     "21"
         MOVE      21             TO   CSV5-MSM04
         MOVE   NC"返品"          TO   CSV5-MSM05
         WHEN     "11"
         MOVE      11             TO   CSV5-MSM04
         MOVE   NC"値引"          TO   CSV5-MSM05
         WHEN     OTHER
         MOVE   ALL "*"           TO   CSV5-MSM04
         MOVE   ALL NC"＊"        TO   CSV5-MSM05
     END-EVALUATE.
     MOVE    ","            TO  CSV5-MSMK04 CSV5-MSMK05.
*伝票番号
     MOVE    SGK-F06        TO  CSV5-MSM06.
     MOVE    ","            TO  CSV5-MSMK06.
*支払金額
     INITIALIZE             WK-HEN081.
     IF      SGK-F05   =    21   OR   11
             COMPUTE   WK-SGK-F07 =   SGK-F07  * (-1)
     ELSE
             MOVE      SGK-F07    TO  WK-SGK-F07
     END-IF.
     IF      WK-SGK-F07  <  ZERO
             MOVE   "-"     TO  WK-HEN081-1
     END-IF.
     MOVE    WK-SGK-F07     TO  WK-HEN08.
     MOVE    WK-HEN08       TO  WK-HEN081-2.
     MOVE    WK-HEN081      TO  CSV5-MSM07.
     MOVE    ","            TO  CSV5-MSMK07.
*備考
     EVALUATE SGK-F09
         WHEN    "20"
              MOVE      NC"２０　支払"      TO   CSV5-MSM08
         WHEN    "30"
              MOVE      NC"３０　支払訂正"  TO   CSV5-MSM08
         WHEN    "40"
              MOVE      NC"４０　未検収"    TO   CSV5-MSM08
         WHEN     OTHER
              MOVE      ALL NC"＊"          TO   CSV5-MSM08
     END-EVALUATE.
     MOVE    ","            TO  CSV5-MSMK08.
*
 CSVGK715-SET-EXIT.
     EXIT.
****************************************************************
*                 店舗マスタ　　READ                           *
****************************************************************
 TENMS1-READ-SEC        SECTION.
     READ     TENMS1    INVALID
              MOVE      "INV"          TO   TENMS1-INV-FLG
              NOT  INVALID
              MOVE      SPACE          TO   TENMS1-INV-FLG
     END-READ.
 TENMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE     RD-CNT     TO      MSG-IN01.
*    MOVE     RD-CNT2    TO      MSG-IN02.
     MOVE     OUT-CNT    TO      MSG-OUT01.
*    MOVE     OUT-CNT2   TO      MSG-OUT02.
     DISPLAY  MSG-IN     UPON    CONS.
     DISPLAY  MSG-OUT    UPON    CONS.
*    DISPLAY  MSG-IN2    UPON    CONS.
*    DISPLAY  MSG-OUT2   UPON    CONS.
*
     CLOSE     SITGG711  SITGK711  TENMS1  CSVGK71.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
