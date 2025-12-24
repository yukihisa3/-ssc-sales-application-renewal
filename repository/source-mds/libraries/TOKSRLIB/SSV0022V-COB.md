# SSV0022V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSV0022V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＤＣＭＪＡＰＡＮ　　　　　　　　　*
*    業務名　　　　　　　：　支払　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払明細ＣＳＶデータ作成　　　　　*
*    作成日／作成者　　　：　2019/03/05 INOUE                  *
*    処理概要　　　　　　：　支払データより　　　　　　　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSV0022V.
*流用元                 SSI8702L.TOKSLIBS
*                       SSY5130V.TOKSRLIB
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/03/05.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払情報データ >>--*
     SELECT   SITME872  ASSIGN         DA-01-VI-SITME872
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F01  SIH-F09
                                       SIH-F02  SIH-F03
                                       SIH-F04  SIH-F05
                        STATUS         SIH-STATUS.
*----<< 店舗マスタ >>--*
     SELECT   TENMS1    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52   TEN-F011
                        STATUS         TEN-STATUS.
*----<< ＣＳＶ　　 >>--*
     SELECT   CSVME87   ASSIGN         DA-01-S-CSVME87
                        ORGANIZATION   SEQUENTIAL
                        ACCESS    MODE SEQUENTIAL
                        STATUS         CSV-STATUS.
************************************************************
 DATA                   DIVISION.
 FILE                   SECTION.
*----<< 支払合計ファイル >>--*
 FD  SITME872           LABEL RECORD   IS   STANDARD.
     COPY     SITME87   OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 店舗マスタ >>--*
 FD  TENMS1             LABEL RECORD   IS   STANDARD.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< ＣＳＶ　　 >>--*
 FD  CSVME87            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  20       RECORDS.
     COPY     CSVME871  OF        XFDLIB
              JOINING   CSV       PREFIX.
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶマルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     CSVME871  OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトル行1
     COPY     CSVME872  OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.明細行
     COPY     CSVME873  OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  SIH-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  SITME872-END-FLG        PIC  X(03)     VALUE  SPACE.
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
 01  WK-HEN                  PIC  9(12).
 01  WK-HEN1.
     03  WK-HEN1-1           PIC  X(01).
     03  WK-HEN1-2           PIC  X(12).
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
         05  ST-PG           PIC  X(08)  VALUE "SSV0022V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSV0022V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSV0022V".
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
 01  LINK-IN-TOKCD           PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING  LINK-IN-SIMEBI
                                         LINK-IN-TOKCD.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SITME872.
     MOVE      "SITME872"   TO   AB-FILE.
     MOVE      SIH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CSVME87.
     MOVE      "CSVME87"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
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
     OPEN     INPUT     SITME872  TENMS1.
     OPEN     OUTPUT    CSVME87.
     DISPLAY  MSG-START UPON CONS.
     DISPLAY "SIMEBI = " LINK-IN-SIMEBI  UPON CONS.
     DISPLAY "TORICD = " LINK-IN-TOKCD   UPON CONS.
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
*INPUT データスタート
     MOVE     SPACE          TO   SIH-REC.
     INITIALIZE                   SIH-REC.
     MOVE     LINK-IN-SIMEBI TO   SIH-F01.
     MOVE     "1"            TO   SIH-F09.
     MOVE     LINK-IN-TOKCD  TO   SIH-F02.
     START  SITME872  KEY  IS  >= SIH-F01  SIH-F09  SIH-F02
                                  SIH-F03  SIH-F04  SIH-F05
            INVALID
            DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
            MOVE    "END"    TO   END-FLG
            MOVE     4010    TO   PROGRAM-STATUS
            GO               TO   INIT-EXIT
     END-START.
*INPUT 読込
     PERFORM SITME872-READ-SEC.
*終了判定
     IF   SITME872-END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　INPUT 読込
****************************************************************
 SITME872-READ-SEC    SECTION.
*
     MOVE    "SITME872-READ-SEC"    TO   S-NAME.
*
     READ     SITME872
              AT  END
                  MOVE     "END"    TO  SITME872-END-FLG
                  GO                TO  SITME872-READ-EXIT
     END-READ.
*
*締日のチェック
     IF    LINK-IN-SIMEBI NOT = SIH-F01
              MOVE     "END"   TO   SITME872-END-FLG
              GO               TO   SITME872-READ-EXIT
     END-IF.
*発注／差異区分
     IF    SIH-F09  =  "2"
              MOVE     "END"   TO   SITME872-END-FLG
              GO               TO   SITME872-READ-EXIT
     END-IF.
*取引先ＣＤチェック
     IF    LINK-IN-TOKCD  =  ZERO
              CONTINUE
     ELSE
           IF  LINK-IN-TOKCD = SIH-F02
               CONTINUE
           ELSE
              MOVE     "END"   TO   SITME872-END-FLG
              GO               TO   SITME872-READ-EXIT
           END-IF
     END-IF.
*
     ADD      1     TO     RD-CNT.
*
     IF   RD-CNT(6:3)  =  "000" OR "500"
          DISPLAY "READ-CNT = " RD-CNT   UPON CONS
     END-IF.
*
 SITME872-READ-EXIT.
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
*****INITIALIZE                      SUM-AREA.
*****INITIALIZE                      CSV-REC.
*
*  ＣＳＶ SET/OUT  1.帳票タイトル行
     PERFORM  CSVME871-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
     MOVE     SPACE       TO      CSV1-REC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  ＣＳＶ SET/OUT  2.項目タイトル行
     PERFORM  CSVME872-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*

 MAIN1-02.
*
*  ＣＳＶ SET/OUT  3.明細行
     PERFORM  CSVME873-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV3-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    INPUT 読込み
     PERFORM  SITME872-READ-SEC.
     IF       SITME872-END-FLG    =    "END"
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
 CSVME871-SET-SEC     SECTION.
*
     MOVE   "CSVME871-SET-SEC"  TO  S-NAME.
*
 CSVME871-SET-01.
*
     MOVE    SPACE                              TO CSV1-REC.
*
     MOVE    X"28"                              TO CSV1-CS00.
     MOVE    NC"【ＤＣＭＪＡＰＡＮ支払明細】"   TO CSV1-C00.
     MOVE    X"29"                              TO CSV1-CE00.
     MOVE    ","                                TO CSV1-CK00.
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
     MOVE    X"28"                              TO CSV1-CS03.
     MOVE    NC"締切日："                       TO CSV1-C03.
     MOVE    X"29"                              TO CSV1-CE03.
     MOVE    ","                                TO CSV1-CK03.
*
     MOVE    SIH-F01(1:4)                       TO CSV1-C04(1:4).
     MOVE    "/"                                TO CSV1-C04(5:1).
     MOVE    SIH-F01(5:2)                       TO CSV1-C04(6:2).
     MOVE    "/"                                TO CSV1-C04(8:1).
     MOVE    SIH-F01(7:2)                       TO CSV1-C04(9:2).
     MOVE    ","                                TO CSV1-CK04.
*
 CSVME8711-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   2.項目タイトル行
****************************************************************
 CSVME872-SET-SEC     SECTION.
*
     MOVE   "CSVME872-SET-SEC"  TO  S-NAME.
*
 CSVME872-SET-01.
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
     MOVE    NC"店舗"                             TO CSV2-K03.
     MOVE    X"29"                                TO CSV2-KE03.
     MOVE    ","                                  TO CSV2-KK03.
*
     MOVE    X"28"                                TO CSV2-KS04.
     MOVE    NC"　"                               TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"納入日"                           TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"伝票番号"                         TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"支払金額"                         TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"摘要"                             TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
 CSVME872-SET-EXIT.
     EXIT.
****************************************************************
*　　　　ＣＳＶ   SET   3.明細行
****************************************************************
 CSVME873-SET-SEC     SECTION.
*
     MOVE     "CSVME873-SET-SEC"  TO  S-NAME.
*
 CSVME873-SET-01.
*
     MOVE    SPACE     TO   CSV3-REC.
*
*制御バイト
     MOVE    X"28"     TO   CSV3-MS02
                            CSV3-MS04
                            CSV3-MS08.
     MOVE    X"29"     TO   CSV3-ME02
                            CSV3-ME04
                            CSV3-ME08.
*取引先ＣＤ
     MOVE    SIH-F02   TO   CSV3-M01.
     MOVE    ","       TO   CSV3-MK01.
*取引先名
     EVALUATE SIH-F02
*----EVALUATE PARA-TORICD
         WHEN 880    MOVE NC"（資材）ＨＣ関東"     TO CSV3-M02
         WHEN 882    MOVE NC"（資材）ＨＣ東北"     TO CSV3-M02
         WHEN 883    MOVE NC"（資材）ＨＣ北海道"   TO CSV3-M02
         WHEN 1427   MOVE NC"（植物）ＨＣ関東"     TO CSV3-M02
         WHEN 14272  MOVE NC"（植物）ＨＣ東北"     TO CSV3-M02
         WHEN 14273  MOVE NC"（植物）ＨＣ北海道"   TO CSV3-M02
         WHEN 13938  MOVE NC"（資材）カーマ西日本" TO CSV3-M02
         WHEN 17137  MOVE NC"（植物）カーマ西日本" TO CSV3-M02
         WHEN 139381 MOVE NC"（資材）カーマ東日本" TO CSV3-M02
         WHEN 171371 MOVE NC"（植物）カーマ東日本" TO CSV3-M02
         WHEN 100403 MOVE NC"ダイキ（資材）４０３" TO CSV3-M02
         WHEN 100441 MOVE NC"ダイキ（資材）４４１" TO CSV3-M02
         WHEN 100427 MOVE NC"ダイキ（植物）４２７" TO CSV3-M02
         WHEN 100404 MOVE NC"サンコ（資材）４０４" TO CSV3-M02
         WHEN 100442 MOVE NC"サンコ（資材）４４２" TO CSV3-M02
         WHEN 100428 MOVE NC"サンコ（植物）４２８" TO CSV3-M02
         WHEN 1731   MOVE NC"ケーヨー資材　東日本" TO CSV3-M02
         WHEN 1732   MOVE NC"ケーヨー資材　東日本" TO CSV3-M02
         WHEN 7601   MOVE NC"ケーヨー植物　西日本" TO CSV3-M02
         WHEN 7602   MOVE NC"ケーヨー植物　西日本" TO CSV3-M02
         WHEN OTHER  MOVE NC"＊＊＊＊＊＊＊＊＊＊" TO CSV3-M02
     END-EVALUATE.
     MOVE    ","       TO   CSV3-MK02.
*店舗ＣＤ
     MOVE    SIH-F03   TO   CSV3-M03.
     MOVE    ","       TO   CSV3-MK03.
*店舗名
     MOVE    SIH-F02   TO   TEN-F52.
     MOVE    SIH-F03   TO   TEN-F011.
     PERFORM TENMS1-READ-SEC.
     IF      TENMS1-INV-FLG = "INV"
             MOVE      ALL NC"＊"     TO   CSV3-M04
     ELSE
             MOVE      TEN-F03        TO   CSV3-M04
     END-IF.
     MOVE    ","       TO   CSV3-MK04.
*納入日
     MOVE    SIH-F04(3:2)   TO  CSV3-M05(1:2).
     MOVE    "."            TO  CSV3-M05(3:1).
     MOVE    SIH-F04(5:2)   TO  CSV3-M05(4:2).
     MOVE    "."            TO  CSV3-M05(6:1).
     MOVE    SIH-F04(7:2)   TO  CSV3-M05(7:2).
     MOVE    ","       TO   CSV3-MK05.
*伝票番号
     MOVE    SIH-F05   TO   CSV3-M06.
     MOVE    ","       TO   CSV3-MK06.
*支払金額
     INITIALIZE             WK-HEN1.
     IF      SIH-F07  <  ZERO
             MOVE "-"  TO   WK-HEN1-1
     END-IF.
     MOVE    SIH-F07   TO   WK-HEN.
     MOVE    WK-HEN    TO   WK-HEN1-2.
     MOVE    WK-HEN1   TO   CSV3-M07.
     MOVE    ","       TO   CSV3-MK07.
*摘要
*    MOVE    SIH-F07   TO   CSV3-M08.
     EVALUATE SIH-F08(1:1)
         WHEN "*"   MOVE NC"未請求" TO  CSV3-M08
         WHEN "#"   MOVE NC"違算　" TO  CSV3-M08
         WHEN "ﾋ"   MOVE NC"非課税" TO  CSV3-M08
         WHEN OTHER MOVE ALL NC"　" TO  CSV3-M08
     END-EVALUATE.
     MOVE    ","       TO   CSV3-MK08.
*
 CSVME873-SET-EXIT.
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
     CLOSE     SITME872  CSVME87  TENMS1.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
