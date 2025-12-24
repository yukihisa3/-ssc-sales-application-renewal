# SSI7807V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSI7807V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ロイヤルＨＣ（インボイス対応）　　*
*    業務名　　　　　　　：　支払　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細ＣＳＶデータ作成　　　　　*
*    作成日／作成者　　　：　2023/11/17 TAKAHASHI              *
*    処理概要　　　　　　：　支払データより　　　　　　　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSI7807V.
*流用元                 SSI7802L.TOKSLIBS
*                       SSV0030V.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/03/06.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払ヘッダデータ >>--*
     SELECT   ROYNHEL1  ASSIGN         DA-01-VI-ROYNHEL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SIH-F03
                        STATUS         SIH-STATUS.
*----<< 支払明細データ >>--*
     SELECT   ROYSMEL1  ASSIGN         DA-01-VI-ROYSMEL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIM-F01  SIM-F02
                                       SIM-F09  SIM-F04
                        STATUS         SIM-STATUS.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-STATUS.
*----<< ＣＳＶ　　 >>--*
     SELECT   CSVROYNH  ASSIGN         DA-01-S-CSVROYNH
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         CSV-STATUS.
************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*----<< 支払ヘッダファイル >>--*
 FD  ROYNHEL1           LABEL RECORD   IS   STANDARD.
     COPY     ROYNHEL1  OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 支払明細データ >>--*
 FD  ROYSMEL1           LABEL RECORD   IS   STANDARD.
     COPY     ROYSMEL1  OF        XFDLIB
              JOINING   SIM       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< ＣＳＶ　　 >>--*
 FD  CSVROYNH            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  4        RECORDS.
     COPY     CSVROYS8  OF        XFDLIB
              JOINING   CSV       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶマルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     CSVROYS8   OF        XFDLIB
              JOINING    CSV1 AS   PREFIX.
* 2.ヘッダタイトル行
     COPY     CSVROYS6   OF        XFDLIB
              JOINING    CSV2 AS   PREFIX.
* 3.ヘッダ行
     COPY     CSVROYS7   OF        XFDLIB
              JOINING    CSV3 AS   PREFIX.
* 4.明細行タイトル行
     COPY     CSVROYS4   OF        XFDLIB
              JOINING    CSV4 AS   PREFIX.
* 5.明細行
     COPY     CSVROYS5   OF        XFDLIB
              JOINING    CSV5 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  SIH-STATUS          PIC  X(02).
     03  SIM-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  ROYNHEL1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  ROYSMEL1-END-FLG        PIC  X(03)     VALUE  SPACE.
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
         05  ST-PG           PIC  X(08)  VALUE "SSI7807V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSI7807V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSI7807V".
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
 LINKAGE                SECTION.
 01  LINK-IN-SIMEBI          PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  LINK-IN-SIMEBI.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYNHEL1.
     MOVE      "ROYNHEL1"   TO   AB-FILE.
     MOVE      SIH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ROYSMEL1.
     MOVE      "ROYSMEL1"   TO   AB-FILE.
     MOVE      SIM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CSVROYNH.
     MOVE      "CSVROYNH"   TO   AB-FILE.
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
     OPEN     INPUT     ROYNHEL1  ROYSMEL1  TENMS1.
     OPEN     OUTPUT    CSVROYNH.
     DISPLAY  MSG-START UPON CONS.
     DISPLAY "SIMEBI = " LINK-IN-SIMEBI  UPON CONS.
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
*INPUT1読込
     MOVE     LINK-IN-SIMEBI    TO   SIH-F03.
     PERFORM  ROYNHEL1-READ-SEC.
     IF       ROYNHEL1-END-FLG  =  "END"
              DISPLAY  NC"＃＃出力対象無し＃＃" UPON CONS
              MOVE    "END"     TO   END-FLG
              MOVE     4010     TO   PROGRAM-STATUS
              GO                TO   INIT-EXIT
     END-IF.
*INPUT2データスタート
     MOVE     SPACE          TO   SIM-REC.
     INITIALIZE                   SIM-REC.
     MOVE     LINK-IN-SIMEBI TO   SIM-F01.
     MOVE     ZERO           TO   SIM-F02 SIM-F09 SIM-F04.
     START    ROYSMEL1 KEY IS  >= SIM-F01 SIM-F02 SIM-F09 SIM-F04
       INVALID
              DISPLAY  NC"＃＃対象データ無し＃＃" UPON CONS
              MOVE    "END"    TO   END-FLG
              MOVE     4010    TO   PROGRAM-STATUS
              GO               TO   INIT-EXIT
     END-START.
*INPUT2読込
     PERFORM  ROYSMEL1-READ-SEC.
     IF       ROYSMEL1-END-FLG  =  "END"
              DISPLAY  NC"＃＃出力対象データ無し＃＃" UPON CONS
              MOVE    "END"     TO    END-FLG
              MOVE     4010     TO    PROGRAM-STATUS
              GO                TO    INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　INPUT1読込
****************************************************************
 ROYNHEL1-READ-SEC    SECTION.
*
     MOVE    "ROYNHEL1-READ-SEC"    TO   S-NAME.
*
     READ     ROYNHEL1
       INVALID
              MOVE    "END"    TO   ROYNHEL1-END-FLG
              GO               TO   ROYNHEL1-READ-EXIT
     END-READ.
*
 ROYNHEL1-READ-EXIT.
     EXIT.
****************************************************************
*　　INPUT2読込
****************************************************************
 ROYSMEL1-READ-SEC    SECTION.
*
     MOVE    "ROYSMEL1-READ-SEC"    TO   S-NAME.
*
     READ     ROYSMEL1
              AT  END
                  MOVE     "END"    TO  ROYSMEL1-END-FLG
                  GO                TO  ROYSMEL1-READ-EXIT
     END-READ.
*
*締日のチェック
     IF    LINK-IN-SIMEBI NOT = SIM-F01
              MOVE     "END"   TO   ROYSMEL1-END-FLG
              GO               TO   ROYSMEL1-READ-EXIT
     END-IF.
*
     ADD      1     TO     RD-CNT.
*
     IF   RD-CNT(6:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 ROYSMEL1-READ-EXIT.
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
     PERFORM  CSVROYNH1-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
     MOVE     SPACE     TO        CSV1-REC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  ＣＳＶ SET/OUT  2.ヘッダタイトル行 (上行)
     PERFORM  CSVROYNH21-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
*  ＣＳＶ SET/OUT  2.ヘッダタイトル行 (下行)
     PERFORM  CSVROYNH22-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*
*  ＣＳＶ SET/OUT  3.ヘッダ行
     PERFORM  CSVROYNH3-SET-SEC.
     WRITE    CSV-REC   FROM      CSV3-REC.
     MOVE     SPACE     TO        CSV3-REC.
     WRITE    CSV-REC   FROM      CSV3-REC.
*
*  ＣＳＶ SET/OUT  4.明細タイトル行
     PERFORM  CSVROYNH4-SET-SEC.
     WRITE    CSV-REC   FROM      CSV4-REC.
*

 MAIN1-02.
*
*  ＣＳＶ SET/OUT  5.明細行
     PERFORM  CSVROYNH5-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV5-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    INPUT2 読込み
     PERFORM  ROYSMEL1-READ-SEC.
     IF       ROYSMEL1-END-FLG    =    "END"
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
 CSVROYNH1-SET-SEC     SECTION.
*
     MOVE   "CSVROYNH1-SET-SEC"  TO  S-NAME.
*
 CSVROYNH1-SET-01.
*
     MOVE    SPACE                              TO CSV1-REC.
*
     MOVE    X"28"                              TO CSV1-CS00.
     MOVE    NC"【ロイヤルホームセンター支払明細】"
                                                TO CSV1-C00.
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
 CSVROYNH1-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   2.ヘッダタイトル行 (上行)
****************************************************************
 CSVROYNH21-SET-SEC     SECTION.
*
     MOVE   "CSVROYNH21-SET-SEC"  TO  S-NAME.
*
 CSVROYNH21-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"☆支払情報"                       TO CSV2-K01.
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
     MOVE    NC"☆当月取引情報"                   TO CSV2-K06.
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
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"　"                               TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"　"                               TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"　"                               TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
     MOVE    X"28"                                TO CSV2-KS13.
     MOVE    NC"　"                               TO CSV2-K13.
     MOVE    X"29"                                TO CSV2-KE13.
     MOVE    ","                                  TO CSV2-KK13.
*
     MOVE    X"28"                                TO CSV2-KS14.
     MOVE    NC"　"                               TO CSV2-K14.
     MOVE    X"29"                                TO CSV2-KE14.
     MOVE    ","                                  TO CSV2-KK14.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"　"                               TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS16.
     MOVE    NC"　"                               TO CSV2-K16.
     MOVE    X"29"                                TO CSV2-KE16.
     MOVE    ","                                  TO CSV2-KK16.
*
     MOVE    X"28"                                TO CSV2-KS17.
     MOVE    NC"　"                               TO CSV2-K16.
     MOVE    X"29"                                TO CSV2-KE17.
     MOVE    ","                                  TO CSV2-KK17.
*
     MOVE    X"28"                                TO CSV2-KS18.
     MOVE    NC"　"                               TO CSV2-K18.
     MOVE    X"29"                                TO CSV2-KE18.
     MOVE    ","                                  TO CSV2-KK18.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"　"                               TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"　"                               TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"　"                               TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"　"                               TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"　"                               TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
 CSVROYNH21-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   2.ヘッダタイトル行 (下行)
****************************************************************
 CSVROYNH22-SET-SEC     SECTION.
*
     MOVE   "CSVROYNH21-SET-SEC"  TO  S-NAME.
*
 CSVROYNH22-SET-01.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-KS01.
     MOVE    NC"締年月日"                         TO CSV2-K01.
     MOVE    X"29"                                TO CSV2-KE01.
     MOVE    ","                                  TO CSV2-KK01.
*
     MOVE    X"28"                                TO CSV2-KS02.
     MOVE    NC"支払年月日"                       TO CSV2-K02.
     MOVE    X"29"                                TO CSV2-KE02.
     MOVE    ","                                  TO CSV2-KK02.
*
     MOVE    X"28"                                TO CSV2-KS03.
     MOVE    NC"対象期間自"                       TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"対象期間至"                       TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"伝票枚数"                         TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"納品"                             TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"納品８％"                         TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"納品１０％"                       TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"返品"                             TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"返品８％"                         TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"返品１０％"                       TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"配送フィー"                       TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
     MOVE    X"28"                                TO CSV2-KS13.
     MOVE    NC"配送フィー８％"                   TO CSV2-K13.
     MOVE    X"29"                                TO CSV2-KE13.
     MOVE    ","                                  TO CSV2-KK13.
*
     MOVE    X"28"                                TO CSV2-KS14.
     MOVE    NC"配送フィー１０％"                 TO CSV2-K14.
     MOVE    X"29"                                TO CSV2-KE14.
     MOVE    ","                                  TO CSV2-KK14.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"買掛計上予定額"                   TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS16.
     MOVE    NC"支払合計"                         TO CSV2-K16.
     MOVE    X"29"                                TO CSV2-KE16.
     MOVE    ","                                  TO CSV2-KK16.
*
     MOVE    X"28"                                TO CSV2-KS17.
     MOVE    NC"税額合計"                         TO CSV2-K17.
     MOVE    X"29"                                TO CSV2-KE17.
     MOVE    ","                                  TO CSV2-KK17.
*
     MOVE    X"28"                                TO CSV2-KS18.
     MOVE    NC"８％支払合計"                    TO CSV2-K18.
     MOVE    X"29"                                TO CSV2-KE18.
     MOVE    ","                                  TO CSV2-KK18.
*
     MOVE    X"28"                                TO CSV2-KS19.
     MOVE    NC"８％税額合計"                     TO CSV2-K19.
     MOVE    X"29"                                TO CSV2-KE19.
     MOVE    ","                                  TO CSV2-KK19.
*
     MOVE    X"28"                                TO CSV2-KS20.
     MOVE    NC"１０％支払合計"                   TO CSV2-K20.
     MOVE    X"29"                                TO CSV2-KE20.
     MOVE    ","                                  TO CSV2-KK20.
*
     MOVE    X"28"                                TO CSV2-KS21.
     MOVE    NC"１０％税額合計"                   TO CSV2-K21.
     MOVE    X"29"                                TO CSV2-KE21.
     MOVE    ","                                  TO CSV2-KK21.
*
     MOVE    X"28"                                TO CSV2-KS22.
     MOVE    NC"当社登録番号"                     TO CSV2-K22.
     MOVE    X"29"                                TO CSV2-KE22.
     MOVE    ","                                  TO CSV2-KK22.
*
     MOVE    X"28"                                TO CSV2-KS23.
     MOVE    NC"取引先登録番号"                   TO CSV2-K23.
     MOVE    X"29"                                TO CSV2-KE23.
*****MOVE    ","                                  TO CSV2-KK23.
*
 CSVROYNH22-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   3.ヘッダ行
****************************************************************
 CSVROYNH3-SET-SEC     SECTION.
*
     MOVE     "CSVROYNH3-SET-SEC"  TO  S-NAME.
*
 CSVROYNH3-SET-01.
*
     MOVE    SPACE     TO   CSV3-REC.
*
*制御バイト
*    MOVE    X"28"     TO   CSV3-MS0X
*                           CSV3-MS0X
*                           CSV3-MS0X.
*    MOVE    X"29"     TO   CSV3-ME0X
*                           CSV3-ME0X
*                           CSV3-ME0X.
*締年月日
     MOVE    SIH-F03(1:4)   TO  CSV3-M01(1:4).
     MOVE    "/"            TO  CSV3-M01(5:1).
     MOVE    SIH-F03(5:2)   TO  CSV3-M01(6:2).
     MOVE    "/"            TO  CSV3-M01(8:1).
     MOVE    SIH-F03(7:2)   TO  CSV3-M01(9:2).
     MOVE    ","            TO  CSV3-MK01.
*支払年月日
     MOVE    SIH-F04(1:4)   TO  CSV3-M02(1:4).
     MOVE    "/"            TO  CSV3-M02(5:1).
     MOVE    SIH-F04(5:2)   TO  CSV3-M02(6:2).
     MOVE    "/"            TO  CSV3-M02(8:1).
     MOVE    SIH-F04(7:2)   TO  CSV3-M02(9:2).
     MOVE    ","            TO  CSV3-MK02.
*対象期間自
     MOVE    SIH-F01(1:4)   TO  CSV3-M03(1:4).
     MOVE    "/"            TO  CSV3-M03(5:1).
     MOVE    SIH-F01(5:2)   TO  CSV3-M03(6:2).
     MOVE    "/"            TO  CSV3-M03(8:1).
     MOVE    SIH-F01(7:2)   TO  CSV3-M03(9:2).
     MOVE    ","            TO  CSV3-MK03.
*対象期間至
     MOVE    SIH-F02(1:4)   TO  CSV3-M04(1:4).
     MOVE    "/"            TO  CSV3-M04(5:1).
     MOVE    SIH-F02(5:2)   TO  CSV3-M04(6:2).
     MOVE    "/"            TO  CSV3-M04(8:1).
     MOVE    SIH-F02(7:2)   TO  CSV3-M04(9:2).
     MOVE    ","            TO  CSV3-MK04.
*伝票枚数
     MOVE    SIH-F05   TO   CSV3-M05.
     MOVE    ","       TO   CSV3-MK05.
*納品
     INITIALIZE             WK-HEN121.
     DISPLAY "SIH-F06 = " SIH-F06 UPON CONS.
     IF      SIH-F06  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F06   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M06.
     DISPLAY "CSV3-M06 = " CSV3-M06 UPON CONS.
     MOVE    ","       TO   CSV3-MK06.
*納品８％
     INITIALIZE             WK-HEN121.
     IF      SIH-F07  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F07   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M07.
     MOVE    ","       TO   CSV3-MK07.
*納品１０％
     INITIALIZE             WK-HEN121.
     IF      SIH-F08  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F08   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M08.
     MOVE    ","       TO   CSV3-MK08.
*返品
     INITIALIZE             WK-HEN121.
     IF      SIH-F09  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F09   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M09.
     MOVE    ","       TO   CSV3-MK09.
*返品８％
     INITIALIZE             WK-HEN121.
     IF      SIH-F10  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F10   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M12.
     MOVE    ","       TO   CSV3-MK10.
*返品１０％
     INITIALIZE             WK-HEN121.
     IF      SIH-F11  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F11   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M11.
     MOVE    ","       TO   CSV3-MK11.
*配送フィー
     INITIALIZE             WK-HEN121.
     IF      SIH-F12  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F12   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M12.
     MOVE    ","       TO   CSV3-MK12.
*配送フィー８％
     INITIALIZE             WK-HEN121.
     IF      SIH-F13  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F13   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M13.
     MOVE    ","       TO   CSV3-MK13.
*配送フィー１０％
     INITIALIZE             WK-HEN121.
     IF      SIH-F14  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F14   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M14.
     MOVE    ","       TO   CSV3-MK14.
*買掛計上予定額
     INITIALIZE             WK-HEN121.
     IF      SIH-F16  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F16   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M15.
     MOVE    ","       TO   CSV3-MK15.
*支払合計
     INITIALIZE             WK-HEN121.
     IF      SIH-F17  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F17   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M16.
     MOVE    ","       TO   CSV3-MK16.
*税額合計
     INITIALIZE             WK-HEN121.
     IF      SIH-F18  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F18   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M17.
     MOVE    ","       TO   CSV3-MK17.
*８％支払合計
     INITIALIZE             WK-HEN121.
     IF      SIH-F19  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F19   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M18.
     MOVE    ","       TO   CSV3-MK18.
*８％税額合計
     INITIALIZE             WK-HEN121.
     IF      SIH-F20  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F20   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M19.
     MOVE    ","       TO   CSV3-MK19.
*１０％支払合計
     INITIALIZE             WK-HEN121.
     IF      SIH-F21  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F21   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M20.
     MOVE    ","       TO   CSV3-MK20.
*１０％税額合計
     INITIALIZE             WK-HEN121.
     IF      SIH-F22  <  ZERO
             MOVE "-"  TO   WK-HEN121-1
     ELSE
             MOVE "0"  TO   WK-HEN121-1
     END-IF.
     MOVE    SIH-F22   TO   WK-HEN12.
     MOVE    WK-HEN12  TO   WK-HEN121-2.
     MOVE    WK-HEN121 TO   CSV3-M21.
     MOVE    ","       TO   CSV3-MK21.
*当社登録番号
     MOVE    SIH-F23   TO   CSV3-M22.
     MOVE    ","       TO   CSV3-MK22.
*取引先登録番号
     MOVE    SIH-F15   TO   CSV3-M23.
*****MOVE    ","       TO   CSV3-MK23.
*
 CSVROYNH3-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   4.明細タイトル行
****************************************************************
 CSVROYNH4-SET-SEC     SECTION.
*
     MOVE   "CSVROYNH4-SET-SEC"  TO  S-NAME.
*
 CSVROYNH4-SET-01.
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
     MOVE    NC"納品日"                           TO CSV4-MTK03.
     MOVE    X"29"                                TO CSV4-MTKE03.
     MOVE    ","                                  TO CSV4-MTKK03.
*
     MOVE    X"28"                                TO CSV4-MTKS04.
     MOVE    NC"検収日"                           TO CSV4-MTK04.
     MOVE    X"29"                                TO CSV4-MTKE04.
     MOVE    ","                                  TO CSV4-MTKK04.
*
     MOVE    X"28"                                TO CSV4-MTKS05.
     MOVE    NC"伝票番号"                         TO CSV4-MTK05.
     MOVE    X"29"                                TO CSV4-MTKE05.
     MOVE    ","                                  TO CSV4-MTKK05.
*
     MOVE    X"28"                                TO CSV4-MTKS06.
     MOVE    NC"伝票区分"                         TO CSV4-MTK06.
     MOVE    X"29"                                TO CSV4-MTKE06.
     MOVE    ","                                  TO CSV4-MTKK06.
*
     MOVE    X"28"                                TO CSV4-MTKS07.
     MOVE    NC"　"                               TO CSV4-MTK07.
     MOVE    X"29"                                TO CSV4-MTKE07.
     MOVE    ","                                  TO CSV4-MTKK07.
*
     MOVE    X"28"                                TO CSV4-MTKS08.
     MOVE    NC"原価金額"                         TO CSV4-MTK08.
     MOVE    X"29"                                TO CSV4-MTKE08.
     MOVE    ","                                  TO CSV4-MTKK08.
*
     MOVE    X"28"                                TO CSV4-MTKS09.
     MOVE    NC"伝票金額"                         TO CSV4-MTK09.
     MOVE    X"29"                                TO CSV4-MTKE09.
     MOVE    ","                                  TO CSV4-MTKK09.
*
     MOVE    X"28"                                TO CSV4-MTKS10.
     MOVE    NC"支払金額"                         TO CSV4-MTK10.
     MOVE    X"29"                                TO CSV4-MTKE10.
     MOVE    ","                                  TO CSV4-MTKK10.
*
     MOVE    X"28"                                TO CSV4-MTKS11.
     MOVE    NC"税額"                             TO CSV4-MTK11.
     MOVE    X"29"                                TO CSV4-MTKE11.
     MOVE    ","                                  TO CSV4-MTKK11.
*
     MOVE    X"28"                                TO CSV4-MTKS12.
     MOVE    NC"発注日"                           TO CSV4-MTK12.
     MOVE    X"29"                                TO CSV4-MTKE12.
     MOVE    ","                                  TO CSV4-MTKK12.
*
     MOVE    X"28"                                TO CSV4-MTKS13.
     MOVE    NC"受領日"                           TO CSV4-MTK13.
     MOVE    X"29"                                TO CSV4-MTKE13.
     MOVE    ","                                  TO CSV4-MTKK13.
*
 CSVROYNH4-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   5.明細行
****************************************************************
 CSVROYNH5-SET-SEC     SECTION.
*
     MOVE     "CSVROYNH5-SET-SEC"  TO  S-NAME.
*
 CSVROYNH5-SET-01.
*
     MOVE    SPACE     TO   CSV5-REC.
*
*制御バイト
     MOVE    X"28"     TO   CSV5-MSMS02
                            CSV5-MSMS07.
     MOVE    X"29"     TO   CSV5-MSME02
                            CSV5-MSME07.
*店舗ＣＤ
     MOVE    SIM-F02   TO   CSV5-MSM01.
     MOVE    ","       TO   CSV5-MSMK01.
*店舗名
     MOVE    51649     TO   TEN-F52.
     MOVE    SIM-F02   TO   TEN-F011.
     PERFORM TENMS1-READ-SEC.
     IF      TENMS1-INV-FLG = "INV"
             MOVE      ALL NC"＊"     TO   CSV5-MSM02
     ELSE
             MOVE      TEN-F03        TO   CSV5-MSM02
     END-IF.
     MOVE    ","       TO   CSV5-MSMK02.
*納入日
     MOVE    SIM-F07(1:4)   TO  CSV5-MSM03(1:4).
     MOVE    "/"            TO  CSV5-MSM03(5:1).
     MOVE    SIM-F07(5:2)   TO  CSV5-MSM03(6:2).
     MOVE    "/"            TO  CSV5-MSM03(8:1).
     MOVE    SIM-F07(7:2)   TO  CSV5-MSM03(9:2).
     MOVE    ","            TO  CSV5-MSMK03.
*検収日
     MOVE    SIM-F09(1:4)   TO  CSV5-MSM04(1:4).
     MOVE    "/"            TO  CSV5-MSM04(5:1).
     MOVE    SIM-F09(5:2)   TO  CSV5-MSM04(6:2).
     MOVE    "/"            TO  CSV5-MSM04(8:1).
     MOVE    SIM-F09(7:2)   TO  CSV5-MSM04(9:2).
     MOVE    ","            TO  CSV5-MSMK04.
*伝票番号
     MOVE    SIM-F04        TO  CSV5-MSM05.
     MOVE    ","            TO  CSV5-MSMK05.
*伝票区分
     MOVE     SIM-F05       TO  CSV5-MSM06.
     MOVE     ","           TO  CSV5-MSMK06.
     EVALUATE SIM-F05
         WHEN "01" MOVE NC"定番"               TO  CSV5-MSM07
         WHEN "02" MOVE NC"特売"               TO  CSV5-MSM07
         WHEN "03" MOVE NC"緊急"               TO  CSV5-MSM07
         WHEN "04" MOVE NC"客注品"             TO  CSV5-MSM07
         WHEN "06" MOVE NC"修理品"             TO  CSV5-MSM07
         WHEN "08" MOVE NC"市場買"             TO  CSV5-MSM07
         WHEN "10" MOVE NC"売上仕入"           TO  CSV5-MSM07
         WHEN "13" MOVE NC"返品"               TO  CSV5-MSM07
         WHEN "15" MOVE NC"原価修正"           TO  CSV5-MSM07
         WHEN "80" MOVE NC"配送Ｃフィー"       TO  CSV5-MSM07
         WHEN "81" MOVE NC"店直フィー"         TO  CSV5-MSM07
         WHEN "82" MOVE NC"店直配送Ｃフィー"   TO  CSV5-MSM07
         WHEN "83" MOVE NC"取引高歩引"         TO  CSV5-MSM07
         WHEN "84" MOVE NC"情報フィー"         TO  CSV5-MSM07
         WHEN "85" MOVE NC"物流改善フィー"     TO  CSV5-MSM07
         WHEN "86" MOVE NC"支払決済歩引"       TO  CSV5-MSM07
         WHEN "87" MOVE NC"新店オープンフィー" TO  CSV5-MSM07
         WHEN "88" MOVE NC"会員セールフィー"   TO  CSV5-MSM07
         WHEN "89" MOVE NC"欠品ペナルティ"     TO  CSV5-MSM07
         WHEN "90" MOVE NC"リベート請求"       TO  CSV5-MSM07
         WHEN "91" MOVE
                   NC"前月以前の返品・相殺過剰未収分の相殺"
                                               TO  CSV5-MSM07
     END-EVALUATE.
     MOVE     ","           TO  CSV5-MSMK07.
*原価金額
     INITIALIZE             WK-HEN111.
     IF      SIM-F10  <  ZERO
             MOVE   "-"     TO  WK-HEN111-1
     END-IF.
     MOVE    SIM-F10        TO  WK-HEN11.
     MOVE    WK-HEN11       TO  WK-HEN111-2.
     MOVE    WK-HEN111      TO  CSV5-MSM08.
     MOVE    ","            TO  CSV5-MSMK08.
*伝票金額
     INITIALIZE             WK-HEN111.
     IF      SIM-F11  <  ZERO
             MOVE   "-"     TO  WK-HEN111-1
     END-IF.
     MOVE    SIM-F11        TO  WK-HEN11.
     MOVE    WK-HEN11       TO  WK-HEN111-2.
     MOVE    WK-HEN111      TO  CSV5-MSM09.
     MOVE    ","            TO  CSV5-MSMK09.
*支払金額
     INITIALIZE             WK-HEN111.
     IF      SIM-F12  <  ZERO
             MOVE   "-"     TO  WK-HEN111-1
     END-IF.
     MOVE    SIM-F12        TO  WK-HEN11.
     MOVE    WK-HEN11       TO  WK-HEN111-2.
     MOVE    WK-HEN111      TO  CSV5-MSM10.
     MOVE    ","            TO  CSV5-MSMK10.
*税額
     INITIALIZE             WK-HEN111.
     IF      SIM-F13  <  ZERO
             MOVE   "-"     TO  WK-HEN111-1
     END-IF.
     MOVE    SIM-F13        TO  WK-HEN11.
     MOVE    WK-HEN11       TO  WK-HEN111-2.
     MOVE    WK-HEN111      TO  CSV5-MSM11.
     MOVE    ","            TO  CSV5-MSMK11.
*発注日
     MOVE    SIM-F06(1:4)   TO  CSV5-MSM12(1:4).
     MOVE    "/"            TO  CSV5-MSM12(5:1).
     MOVE    SIM-F06(5:2)   TO  CSV5-MSM12(6:2).
     MOVE    "/"            TO  CSV5-MSM12(8:1).
     MOVE    SIM-F06(7:2)   TO  CSV5-MSM12(9:2).
     MOVE    ","            TO  CSV5-MSMK12.
*受領日
     MOVE    SIM-F08(1:4)   TO  CSV5-MSM13(1:4).
     MOVE    "/"            TO  CSV5-MSM13(5:1).
     MOVE    SIM-F08(5:2)   TO  CSV5-MSM13(6:2).
     MOVE    "/"            TO  CSV5-MSM13(8:1).
     MOVE    SIM-F08(7:2)   TO  CSV5-MSM13(9:2).
     MOVE    ","            TO  CSV5-MSMK13.
*
 CSVROYNH5-SET-EXIT.
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
     CLOSE     ROYNHEL1  ROYSMEL1  TENMS1  CSVROYNH.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
