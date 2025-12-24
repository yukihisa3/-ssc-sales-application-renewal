****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　作場・数量一括変更　　　　　　　　*
*    　　　　　　　　　　　　ＣＳＶデータ作成　　　　　　　　  *
*    作成日／作成者　　　：　2019/02/18 INOUE                  *
*    処理概要　　　　　　：　作場・数量一括変更ワークより　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　                                  *
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5130V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2019/02/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*一括変更出力ＷＫ
     SELECT   CHGXXXW   ASSIGN    TO        DA-01-S-CHGXXXW
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CHG-STATUS.
*一括変更ＣＳＶ
     SELECT   CHGXXXF   ASSIGN    TO        DA-01-CHGXXXF
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*一括変更出力ＷＫ
******************************************************************
 FD  CHGXXXW           LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  11        RECORDS.
     COPY     CHGXXXW   OF        XFDLIB
              JOINING   CHG  AS   PREFIX.
*
******************************************************************
*一括変更ＣＳＶ
******************************************************************
 FD  CHGXXXF            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  11       RECORDS.
     COPY     CHGXXX1   OF        XFDLIB
              JOINING   CSV       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*振分ＣＳＶ（明細）マルチレイアウト用ＣＯＰＹ句
* 1.帳票タイトル行
     COPY     CHGXXX1   OF        XFDLIB
              JOINING   CSV1 AS   PREFIX.
* 2.項目タイトル行1
     COPY     CHGXXX2   OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 3.明細行
     COPY     CHGXXX3   OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  CHG-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  CHGXXXW-END-FLG         PIC  X(03)     VALUE  SPACE.
 01  TENPO-HIT-FLG           PIC  X(03)     VALUE  SPACE.
 01  KEY-BRK-FLG             PIC  X(03)     VALUE  SPACE.
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  RD-CNT2             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT2            PIC  9(08)     VALUE  ZERO.
*店舗テーブル領域用
*01  TENPO-IX                PIC  9(03)     VALUE  ZERO.
*01  TENPO-TBL.
*  03  TENPO-CD              PIC  9(05)     OCCURS 500.
*店舗テーブル合計数量
*01  GK-SURYO                PIC  9(10)     VALUE  ZERO.
*
*数量編集用
 01  WK-CHG-F13              PIC  9(06)     VALUE  ZERO.
 01  SUM-AREA.
  02 SUM-M161                PIC  9(06)     OCCURS 500.
*ブレイク領域
* 一括変更ＣＳＶ用
*01  BR-TENPOCD              PIC  9(05)     VALUE  ZERO.
* 振分ＣＳＶ（明細）用
*01  BR-SOUKOCD              PIC  X(02)     VALUE  SPACE.
*01  BR-BUMONCD              PIC  X(04)     VALUE  SPACE.
*01  BR-SYOBUNCD             PIC  X(08)     VALUE  SPACE.
*01  BR-NOUHINBI             PIC  9(08)     VALUE  ZERO.
*01  BR-JISYASYOCD           PIC  X(16)     VALUE  SPACE.
*01  BR-AITESYOCD            PIC  X(13)     VALUE  SPACE.
*01  BR-PATNCD               PIC  X(02)     VALUE  SPACE.
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
         05  ST-PG           PIC  X(08)  VALUE "SSY5130V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5130V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5130V".
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
*01  LINK-JUN                PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CHGXXXW.
     MOVE      "CHGXXXW"   TO   AB-FILE.
     MOVE      CHG-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CHGXXXF.
     MOVE      "CHGXXXF"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
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
     OPEN     INPUT     CHGXXXW.
     OPEN     OUTPUT    CHGXXXF.
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
*一括変更出力ＷＫ読込
     PERFORM CHGXXXW-READ-SEC.
*終了判定
     IF   CHGXXXW-END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
*    ELSE
*         INITIALIZE              TENPO-TBL
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　一括変更出力ＷＫ読込　　　　　　　　　　　　　　　　　　　*
****************************************************************
 CHGXXXW-READ-SEC    SECTION.
*
     MOVE    "CHGXXXW-READ-SEC"    TO   S-NAME.
*
     READ     CHGXXXW
              AT  END
                  MOVE     "END"    TO  CHGXXXW-END-FLG
                  GO                TO  CHGXXXW-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
 CHGXXXW-READ-EXIT.
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
     INITIALIZE                      SUM-AREA.
*****INITIALIZE                      CSV-REC.
*
*  一括変更ＣＳＶ SET/OUT  1.帳票タイトル行
     PERFORM  CHGXXX1-SET-SEC.
     WRITE    CSV-REC   FROM      CSV1-REC.
*
*  一括変更ＣＳＶ SET/OUT  2.項目タイトル行
     PERFORM  CHGXXX2-SET-SEC.
     WRITE    CSV-REC   FROM      CSV2-REC.
*

 MAIN1-02.
*
*  一括変更ＣＳＶ SET/OUT  3.明細行
     PERFORM  CHGXXX3-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV3-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    一括変更ワーク読込み
     PERFORM  CHGXXXW-READ-SEC.
     IF       CHGXXXW-END-FLG    =    "END"
              MOVE    "END"      TO    END-FLG
              GO                 TO    MAIN1-EXIT
     END-IF.
*
     GO       TO       MAIN1-02.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*一括変更ＣＳＶ  SET   1.帳票タイトル行
****************************************************************
 CHGXXX1-SET-SEC     SECTION.
*
     MOVE   "CHGXXX1-SET-SEC"  TO  S-NAME.
*
 CHGXXX1-SET-01.
*
     MOVE    SPACE                                TO CSV1-REC.
*
     MOVE    X"28"                                TO CSV1-CS00.
     MOVE    NC"作場・数量一括変更ＥＸＣＥＬ　　" TO CSV1-C00.
     MOVE    X"29"                                TO CSV1-CE00.
     MOVE    ","                                  TO CSV1-CK00.
*
 CHGXXX11-SET-EXIT.
     EXIT.
****************************************************************
*一括変更ＣＳＶ   SET   2.帳票タイトル行
****************************************************************
 CHGXXX2-SET-SEC     SECTION.
*
     MOVE   "CHGXXX2-SET-SEC"  TO  S-NAME.
*
 CHGXXX2-SET-01.
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
     MOVE    NC"バッチ取引先"                     TO CSV2-K04.
     MOVE    X"29"                                TO CSV2-KE04.
     MOVE    ","                                  TO CSV2-KK04.
*
     MOVE    X"28"                                TO CSV2-KS05.
     MOVE    NC"納品日"                           TO CSV2-K05.
     MOVE    X"29"                                TO CSV2-KE05.
     MOVE    ","                                  TO CSV2-KK05.
*
     MOVE    X"28"                                TO CSV2-KS06.
     MOVE    NC"伝票番号"                         TO CSV2-K06.
     MOVE    X"29"                                TO CSV2-KE06.
     MOVE    ","                                  TO CSV2-KK06.
*
     MOVE    X"28"                                TO CSV2-KS07.
     MOVE    NC"行番号"                           TO CSV2-K07.
     MOVE    X"29"                                TO CSV2-KE07.
     MOVE    ","                                  TO CSV2-KK07.
*
     MOVE    X"28"                                TO CSV2-KS08.
     MOVE    NC"量販店商品ＣＤ"                   TO CSV2-K08.
     MOVE    X"29"                                TO CSV2-KE08.
     MOVE    ","                                  TO CSV2-KK08.
*
     MOVE    X"28"                                TO CSV2-KS09.
     MOVE    NC"サカタコード"                     TO CSV2-K09.
     MOVE    X"29"                                TO CSV2-KE09.
     MOVE    ","                                  TO CSV2-KK09.
*
     MOVE    X"28"                                TO CSV2-KS10.
     MOVE    NC"ＪＡＮＣＤ"                       TO CSV2-K10.
     MOVE    X"29"                                TO CSV2-KE10.
     MOVE    ","                                  TO CSV2-KK10.
*
     MOVE    X"28"                                TO CSV2-KS11.
     MOVE    NC"商品名１"                         TO CSV2-K11.
     MOVE    X"29"                                TO CSV2-KE11.
     MOVE    ","                                  TO CSV2-KK11.
*
     MOVE    X"28"                                TO CSV2-KS12.
     MOVE    NC"商品名２"                         TO CSV2-K12.
     MOVE    X"29"                                TO CSV2-KE12.
     MOVE    ","                                  TO CSV2-KK12.
*
     MOVE    X"28"                                TO CSV2-KS13.
     MOVE    NC"原価単価"                         TO CSV2-K13.
     MOVE    X"29"                                TO CSV2-KE13.
     MOVE    ","                                  TO CSV2-KK13.
*
     MOVE    X"28"                                TO CSV2-KS14.
     MOVE    NC"売価単価"                         TO CSV2-K14.
     MOVE    X"29"                                TO CSV2-KE14.
     MOVE    ","                                  TO CSV2-KK14.
*
     MOVE    X"28"                                TO CSV2-KS15.
     MOVE    NC"商品分類ＣＤ"                     TO CSV2-K15.
     MOVE    X"29"                                TO CSV2-KE15.
     MOVE    ","                                  TO CSV2-KK15.
*
     MOVE    X"28"                                TO CSV2-KS16.
     MOVE    NC"店舗ＣＤ"                         TO CSV2-K16.
     MOVE    X"29"                                TO CSV2-KE16.
     MOVE    ","                                  TO CSV2-KK16.
*
     MOVE    X"28"                                TO CSV2-KS17.
     MOVE    NC"店舗名"                           TO CSV2-K17.
     MOVE    X"29"                                TO CSV2-KE17.
     MOVE    ","                                  TO CSV2-KK17.
*
     MOVE    X"28"                                TO CSV2-KS18.
     MOVE    NC"パターンＣＤ"                     TO CSV2-K18.
     MOVE    X"29"                                TO CSV2-KE18.
     MOVE    ","                                  TO CSV2-KK18.
*
     MOVE    X"28"                                TO CSV2-KS19.
     MOVE    NC"県ＣＤ"                           TO CSV2-K19.
     MOVE    X"29"                                TO CSV2-KE19.
     MOVE    ","                                  TO CSV2-KK19.
*
     MOVE    X"28"                                TO CSV2-KS20.
     MOVE    NC"エリアＣＤ"                       TO CSV2-K20.
     MOVE    X"29"                                TO CSV2-KE20.
     MOVE    ","                                  TO CSV2-KK20.
*
     MOVE    X"28"                                TO CSV2-KS21.
     MOVE    NC"入数"                             TO CSV2-K21.
     MOVE    X"29"                                TO CSV2-KE21.
     MOVE    ","                                  TO CSV2-KK21.
*
     MOVE    X"28"                                TO CSV2-KS22.
     MOVE    NC"発注数"                           TO CSV2-K22.
     MOVE    X"29"                                TO CSV2-KE22.
     MOVE    ","                                  TO CSV2-KK22.
*
     MOVE    X"28"                                TO CSV2-KS23.
     MOVE    NC"訂正数"                           TO CSV2-K23.
     MOVE    X"29"                                TO CSV2-KE23.
     MOVE    ","                                  TO CSV2-KK23.
*
     MOVE    X"28"                                TO CSV2-KS24.
     MOVE    NC"倉庫ＣＤ"                         TO CSV2-K24.
     MOVE    X"29"                                TO CSV2-KE24.
     MOVE    ","                                  TO CSV2-KK24.
*
     MOVE    X"28"                                TO CSV2-KS25.
     MOVE    NC"変更作場"                         TO CSV2-K25.
     MOVE    X"29"                                TO CSV2-KE25.
     MOVE    ","                                  TO CSV2-KK25.
*
     MOVE    X"28"                                TO CSV2-KS26.
     MOVE    NC"変更区分"                         TO CSV2-K26.
     MOVE    X"29"                                TO CSV2-KE26.
     MOVE    ","                                  TO CSV2-KK26.
*
*    MOVE    ALL ","                              TO CSV2-KK16.
*
 CHGXXX2-SET-EXIT.
     EXIT.
****************************************************************
*一括変更ＣＳＶ   SET   3.明細行
****************************************************************
 CHGXXX3-SET-SEC     SECTION.
*
     MOVE     "CHGXXX3-SET-SEC"  TO  S-NAME.
*
 CHGXXX3-SET-01.
*
*制御バイト
     MOVE    X"28"     TO   CSV3-MK11S1
                            CSV3-MK12S1
                            CSV3-MK17S1.
     MOVE    X"29"     TO   CSV3-MK11S2
                            CSV3-MK12S2
                            CSV3-MK17S2.
*管理番号
     MOVE    CHG-F23   TO   CSV3-M01.
     MOVE    ","       TO   CSV3-MK01.
*バッチ日付
     MOVE    CHG-F011  TO   CSV3-M02.
     MOVE    ","       TO   CSV3-MK02.
*バッチ時刻
     MOVE    CHG-F012  TO   CSV3-M03.
     MOVE    ","       TO   CSV3-MK03.
*バッチ取引先
     MOVE    CHG-F013  TO   CSV3-M04.
     MOVE    ","       TO   CSV3-MK04.
*納品日
     MOVE    CHG-F06   TO   CSV3-M05.
     MOVE    ","       TO   CSV3-MK05.
*伝票番号
     MOVE    CHG-F28   TO   CSV3-M06.
     MOVE    ","       TO   CSV3-MK06.
*行番号
     MOVE    CHG-F29   TO   CSV3-M07.
     MOVE    ","       TO   CSV3-MK07.
*量販店商品ＣＤ
     MOVE    CHG-F07   TO   CSV3-M08.
     MOVE    ","       TO   CSV3-MK08.
*サカタコード
     MOVE    CHG-F081  TO   CSV3-M09.
     MOVE    ","       TO   CSV3-MK09.
*ＪＡＮＣＤ
     MOVE    CHG-F26   TO   CSV3-M10.
     MOVE    ","       TO   CSV3-MK10.
*商品名１
     MOVE    CHG-F111  TO   CSV3-M11.
     MOVE    ","       TO   CSV3-MK11.
*商品名２
     MOVE    CHG-F112  TO   CSV3-M12.
     MOVE    ","       TO   CSV3-MK12.
*原価単価
*#2020/06/30 NAV ST ゼロセット
*****MOVE    CHG-F15   TO   CSV3-M13.
     MOVE    ZERO      TO   CSV3-M13.
*#2020/06/30 NAV ED ゼロセット
     MOVE    ","       TO   CSV3-MK13.
*売価単価
     MOVE    CHG-F16   TO   CSV3-M14.
     MOVE    ","       TO   CSV3-MK14.
*商品分類ＣＤ
     MOVE    CHG-F24   TO   CSV3-M15.
     MOVE    ","       TO   CSV3-MK15.
*店舗ＣＤ
     MOVE    CHG-F02   TO   CSV3-M16.
     MOVE    ","       TO   CSV3-MK16.
*店舗名
     MOVE    CHG-F30   TO   CSV3-M17.
     MOVE    ","       TO   CSV3-MK17.
*パターンＣＤ
     MOVE    CHG-F25   TO   CSV3-M18.
     MOVE    ","       TO   CSV3-MK18.
*県ＣＤ
     MOVE    CHG-F31   TO   CSV3-M19.
     MOVE    ","       TO   CSV3-MK19.
*エリアＣＤ
     MOVE    CHG-F32   TO   CSV3-M20.
     MOVE    ","       TO   CSV3-MK20.
*入数
     MOVE    CHG-F27   TO   CSV3-M21.
     MOVE    ","       TO   CSV3-MK21.
*発注数
     MOVE    CHG-F14   TO   CSV3-M22.
     MOVE    ","       TO   CSV3-MK22.
*訂正数
     MOVE    CHG-F34   TO   CSV3-M23.
     MOVE    ","       TO   CSV3-MK23.
*倉庫ＣＤ
     MOVE    CHG-F03   TO   CSV3-M24.
     MOVE    ","       TO   CSV3-MK24.
*変更作場
     MOVE    CHG-F33   TO   CSV3-M25.
     MOVE    ","       TO   CSV3-MK25.
*変更区分
     MOVE    CHG-F35   TO   CSV3-M26.
     MOVE    ","       TO   CSV3-MK26.
*    MOVE    ","       TO   CSV3-MFIL00(36:1).
*
 CHGXXX3-SET-EXIT.
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
*    CLOSE     NFTENMS1.
     CLOSE     CHGXXXW  CHGXXXF.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
