# SSY5210V

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY5210V.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ新ＥＤＩ　　　　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援　　　　　　　　　　*
*    モジュール名　　　　：　出荷確定データＣＳＶ出力　　　　　*
*    作成日／作成者　　　：　2020/02/21 NAV                    *
*    処理概要　　　　　　：　数量訂正ファイルより　　　　　　　*
*                            ＰＣへ転送するＣＳＶデータを　　　*
*                            出力する。　　　　　              *
*    更新日／更新者　　　：　2020/02/25 NAV TAKAHASHI          *
*    処理概要　　　　　　：　作場名追加、原価単価削除　　　　　*
*                            （作場Ｍ追加）　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY5210V.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/02/21.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*数量訂正ファイル
     SELECT   NFSUTEL1  ASSIGN    TO        DA-01-VI-NFSUTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       IS   SUT-F01
                                                 SUT-F05
                                                 SUT-F06
                                                 SUT-F07
                                                 SUT-F08
                                                 SUT-F09
                        FILE      STATUS    SUT-STATUS.
*---<<  ナフコ商品マスタ  >>---*
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHO-F01
                        FILE      STATUS    IS   SHO-STATUS.
*---<<  ナフコ店舗マスタ  >>---*
     SELECT   NFTENMS1  ASSIGN    TO        DA-01-VI-NFTENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F01 TEN-F02
                        FILE      STATUS    IS   TEN-STATUS.
*---<<  ナフコ作場マスタ  >>---*
     SELECT   SAKUBAL1  ASSIGN    TO        DA-01-VI-SAKUBAL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SAK-F01
                        FILE      STATUS    IS   SAK-STATUS.
*ＣＳＶ
     SELECT   NFKATCSV  ASSIGN    TO        DA-01-NFKATCSV
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CSV-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*数量訂正ファイル
******************************************************************
 FD  NFSUTEL1           LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEL1  OF        XFDLIB
              JOINING   SUT  AS   PREFIX.
*
******************************************************************
*商品マスタ
******************************************************************
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO  AS   PREFIX.
*
******************************************************************
*店舗マスタ
******************************************************************
 FD  NFTENMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN  AS   PREFIX.
*
******************************************************************
*作場マスタ
******************************************************************
 FD  SAKUBAL1           LABEL RECORD   IS   STANDARD.
     COPY     SAKUBAL1  OF        XFDLIB
              JOINING   SAK  AS   PREFIX.
*
******************************************************************
*ＣＳＶ
******************************************************************
 FD  NFKATCSV            LABEL RECORD   IS   STANDARD
     BLOCK    CONTAINS  8       RECORDS.
     COPY     NFKATCSV  OF        XFDLIB
              JOINING   CSV       PREFIX.

*****************************************************************
*
 WORKING-STORAGE        SECTION.
*
*ＣＳＶマルチレイアウト用ＣＯＰＹ句
* 項目タイトル行1
     COPY     NFKATTTL   OF        XFDLIB
              JOINING   CSV2 AS   PREFIX.
* 明細行
     COPY     NFKATCSV   OF        XFDLIB
              JOINING   CSV3 AS   PREFIX.
*
*ＳＴＡＴＵＳ領域
 01  WK-ST.
     03  SUT-STATUS          PIC  X(02).
     03  CSV-STATUS          PIC  X(02).
     03  SHO-STATUS          PIC  X(02).
     03  TEN-STATUS          PIC  X(02).
     03  SAK-STATUS          PIC  X(02).
*フラグ領域
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  NFTENMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFSHOMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  SAKUBAL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
*抽出条件
 01  WK-RD-KANRNO           PIC  9(08).
 01  TBL-AREA.
     03  TBL-SAKBCD-AREA    OCCURS 20  INDEXED BY TBL-IX.
        05  TBL-SAKBCD      PIC  X(02).
*店舗マスタ退避
 01  WK-TEN-AREA.
     03  WK-TEN-F01         PIC  9(08)  VALUE ZERO.
     03  WK-TEN-F02         PIC  9(05)  VALUE ZERO.
*
*
*カウンター領域
 01  WK-CNT.
     03  RD-CNT              PIC  9(08)     VALUE  ZERO.
     03  SEL-CNT             PIC  9(08)     VALUE  ZERO.
     03  OUT-CNT             PIC  9(08)     VALUE  ZERO.
*
*数字編集用
 01  WK-TANKA-X.
     03  WK-TANKA            PIC  9(07).99.
 01  WK-TANI-X.
     03  WK-TANI             PIC  9(06).9.
 01  WK-RITU-X.
     03  WK-RITU             PIC  9(03).9.
*システム日付編集領域
 01  WK-AREA.
     03  SYS-DATE            PIC  9(06).
     03  SYS-DATEW           PIC  9(08).
*メッセージ出力領域
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  ST-PG           PIC  X(08)  VALUE "SSY5210V".
         05  FILLER          PIC  X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5210V".
         05  FILLER          PIC  X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC  X(05)  VALUE " *** ".
         05  END-PG          PIC  X(08)  VALUE "SSY5210V".
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
*日付変換サブルーチン用領域
 01  LINK-AREA.
     03  LINK-IN-KBN         PIC   X(01).
     03  LINK-IN-YMD6        PIC   9(06).
     03  LINK-IN-YMD8        PIC   9(08).
     03  LINK-OUT-RET        PIC   X(01).
     03  LINK-OUT-YMD8       PIC   9(08).
*
 LINKAGE                SECTION.
 01  PA-KANRNO    PIC  9(08). *> 管理番号
 01  PA-SAKBCD    PIC  X(40). *> 作場ＣＤ  X(2) OCCURS 20
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION  USING
                                             PA-KANRNO
                                             PA-SAKBCD.
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEL1.
     MOVE      "NFSUTEL1"   TO   AB-FILE.
     MOVE      SUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFKATCSV.
     MOVE      "NFKATCSV"   TO   AB-FILE.
     MOVE      CSV-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

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

     OPEN     INPUT     NFSUTEL1 NFSHOMS1 NFTENMS1  SAKUBAL1.
     OPEN     OUTPUT    NFKATCSV.
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
         MOVE    "20"       TO   SYS-DATEW(1:2).
         MOVE    SYS-DATE   TO   SYS-DATEW(3:6).
*
* 初期値設定
     INITIALIZE              WK-TEN-AREA.
     MOVE  PA-KANRNO        TO  WK-RD-KANRNO.
     MOVE  PA-SAKBCD        TO  TBL-AREA.
*
*数量訂正ファイルＳＴＡＲＴ
     MOVE    SPACE          TO    SUT-REC.
     INITIALIZE                   SUT-REC.
     MOVE    WK-RD-KANRNO   TO    SUT-F01.
     START   NFSUTEL1  KEY IS >= SUT-F01 SUT-F05 SUT-F06
                                 SUT-F07 SUT-F08 SUT-F09
      INVALID
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
          GO                TO    INIT-EXIT
     END-START.
*数量訂正ファイル読込み
     PERFORM NFSUTEL1-READ-SEC.
*
*終了判定
     IF   END-FLG  =  "END"
          DISPLAY "＃＃出力対象データ無し＃＃" UPON CONS
          MOVE    "END"     TO    END-FLG
          MOVE     4010     TO    PROGRAM-STATUS
     ELSE
*  ＣＳＶ　　編集・出力　　 2.項目タイトル行
          PERFORM  CSVXXX2-SET-SEC
          WRITE    CSV-REC   FROM      CSV2-REC
*  ＣＳＶ　　初期値設定　　 3.明細
          PERFORM  CSVXXX3-INIT-SEC
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　数量訂正Ｆ読込　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 NFSUTEL1-READ-SEC    SECTION.
*
     MOVE    "NFSUTEL1-READ-SEC"    TO   S-NAME.
*
 NFSUTEL1-READ-01.
     READ     NFSUTEL1
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  NFSUTEL1-READ-EXIT
     END-READ.
*
     ADD      1     TO     RD-CNT.
*
*条件判定
*管理番号チェック
     IF  SUT-F01 > WK-RD-KANRNO
         MOVE  "END"        TO  END-FLG
         GO TO  NFSUTEL1-READ-EXIT
     END-IF.
*作場ＣＤチェック
     IF  PA-SAKBCD = SPACE
         CONTINUE
     ELSE
         SET  TBL-IX  TO  1
         SEARCH  TBL-SAKBCD-AREA
             AT END
                   GO TO  NFSUTEL1-READ-01
             WHEN  TBL-SAKBCD(TBL-IX) = SUT-F05
                   CONTINUE
         END-SEARCH
     END-IF.
*発注確定済チェック
     IF  SUT-F91 =  "1"
         CONTINUE
     ELSE
         GO TO  NFSUTEL1-READ-01
     END-IF.
*
     ADD      1     TO     SEL-CNT.
*
 NFSUTEL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理１　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN1-SEC     SECTION.
*
     MOVE    "MAIN1-SEC"     TO      S-NAME.
*
*  ＣＳＶ　　編集　　　 3.明細行
     PERFORM  CSVXXX3-SET-SEC.
*    レコード出力
     WRITE    CSV-REC   FROM  CSV3-REC.
*    出力件数カウント
     ADD      1         TO    OUT-CNT.
*
*    数量訂正ファイル読込み
     PERFORM NFSUTEL1-READ-SEC.
*
 MAIN1-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ  SET   1.帳票タイトル行
****************************************************************
*CSVXXX1-SET-SEC     SECTION.
*
*    MOVE   "CSVXXX1-SET-SEC"  TO  S-NAME.
*
*    MOVE    SPACE                                TO CSV1-REC.
*
*    MOVE    X"28"                                TO CSV1-CS00.
*    MOVE    NC"ナフコ　発注一覧"                 TO CSV1-C00.
*    MOVE    X"29"                                TO CSV1-CE00.
*    MOVE    ","                                  TO CSV1-CK00.
*
*CSVXXX11-SET-EXIT.
*    EXIT.
****************************************************************
*ＣＳＶ   SET   2.帳票タイトル行
****************************************************************
 CSVXXX2-SET-SEC     SECTION.
*
     MOVE   "CSVXXX2-SET-SEC"  TO  S-NAME.
*
     MOVE    SPACE                                TO CSV2-REC.
*
     MOVE    X"28"                                TO CSV2-F01S.
     MOVE    NC"管理番号"                         TO CSV2-F01.
     MOVE    X"29"                                TO CSV2-F01E.
     MOVE    ","                                  TO CSV2-A01.
*
     MOVE    X"28"                                TO CSV2-F02S.
     MOVE    NC"受信日"                           TO CSV2-F02.
     MOVE    X"29"                                TO CSV2-F02E.
     MOVE    ","                                  TO CSV2-A02.
*
     MOVE    X"28"                                TO CSV2-F03S.
     MOVE    NC"受信時間"                         TO CSV2-F03.
     MOVE    X"29"                                TO CSV2-F03E.
     MOVE    ","                                  TO CSV2-A03.
*
     MOVE    X"28"                                TO CSV2-F04S.
     MOVE    NC"取引先ＣＤ"                       TO CSV2-F04.
     MOVE    X"29"                                TO CSV2-F04E.
     MOVE    ","                                  TO CSV2-A04.
*
     MOVE    X"28"                                TO CSV2-F05S.
     MOVE    NC"作場"                             TO CSV2-F05.
     MOVE    X"29"                                TO CSV2-F05E.
     MOVE    ","                                  TO CSV2-A05.
*
     MOVE    X"28"                                TO CSV2-F06S.
     MOVE    NC"店舗ＣＤ"                         TO CSV2-F06.
     MOVE    X"29"                                TO CSV2-F06E.
     MOVE    ","                                  TO CSV2-A06.
*
     MOVE    X"28"                                TO CSV2-F07S.
     MOVE    NC"店舗名"                           TO CSV2-F07.
     MOVE    X"29"                                TO CSV2-F07E.
     MOVE    ","                                  TO CSV2-A07.
*
     MOVE    X"28"                                TO CSV2-F08S.
     MOVE    NC"納品場所"                         TO CSV2-F08.
     MOVE    X"29"                                TO CSV2-F08E.
     MOVE    ","                                  TO CSV2-A08.
*
     MOVE    X"28"                                TO CSV2-F09S.
     MOVE    NC"県コード"                         TO CSV2-F09.
     MOVE    X"29"                                TO CSV2-F09E.
     MOVE    ","                                  TO CSV2-A09.
*
     MOVE    X"28"                                TO CSV2-F10S.
     MOVE    NC"出荷日"                           TO CSV2-F10.
     MOVE    X"29"                                TO CSV2-F10E.
     MOVE    ","                                  TO CSV2-A10.
*
     MOVE    X"28"                                TO CSV2-F11S.
     MOVE    NC"店着日"                           TO CSV2-F11.
     MOVE    X"29"                                TO CSV2-F11E.
     MOVE    ","                                  TO CSV2-A11.
*
     MOVE    X"28"                                TO CSV2-F12S.
     MOVE    NC"伝票番号"                         TO CSV2-F12.
     MOVE    X"29"                                TO CSV2-F12E.
     MOVE    ","                                  TO CSV2-A12.
*
     MOVE    X"28"                                TO CSV2-F13S.
     MOVE    NC"ナフコ商品ＣＤ"                   TO CSV2-F13.
     MOVE    X"29"                                TO CSV2-F13E.
     MOVE    ","                                  TO CSV2-A13.
*
     MOVE    X"28"                                TO CSV2-F14S.
     MOVE    NC"ＪＡＮＣＤ"                       TO CSV2-F14.
     MOVE    X"29"                                TO CSV2-F14E.
     MOVE    ","                                  TO CSV2-A14.
*
     MOVE    X"28"                                TO CSV2-F15S.
     MOVE    NC"商品名"                           TO CSV2-F15.
     MOVE    X"29"                                TO CSV2-F15E.
     MOVE    ","                                  TO CSV2-A15.
*
     MOVE    X"28"                                TO CSV2-F16S.
     MOVE    NC"規格名"                           TO CSV2-F16.
     MOVE    X"29"                                TO CSV2-F16E.
     MOVE    ","                                  TO CSV2-A16.
*
     MOVE    X"28"                                TO CSV2-F17S.
     MOVE    NC"納品数"                           TO CSV2-F17.
     MOVE    X"29"                                TO CSV2-F17E.
     MOVE    ","                                  TO CSV2-A17.
*
     MOVE    X"28"                                TO CSV2-F18S.
     MOVE    NC"発注単位"                         TO CSV2-F18.
     MOVE    X"29"                                TO CSV2-F18E.
     MOVE    ","                                  TO CSV2-A18.
*#2020/02/25 NAV ST　原価単価削除／作場明追加
**   MOVE    X"28"                                TO CSV2-F19S.
**   MOVE    NC"原価単価"                         TO CSV2-F19.
**   MOVE    X"29"                                TO CSV2-F19E.
**   MOVE    ","                                  TO CSV2-A19.
**
     MOVE    X"28"                                TO CSV2-F051S.
     MOVE    NC"作場名"                           TO CSV2-F051.
     MOVE    X"29"                                TO CSV2-F051E.
     MOVE    ","                                  TO CSV2-A051.
*#2020/02/25 NAV ED　原価単価削除／作場明追加
*
     MOVE    X"28"                                TO CSV2-F20S.
     MOVE    NC"売価単価"                         TO CSV2-F20.
     MOVE    X"29"                                TO CSV2-F20E.
*    MOVE    ","                                  TO CSV2-A20.
*
 CSVXXX2-SET-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   初期値設定   3.明細行
****************************************************************
 CSVXXX3-INIT-SEC    SECTION.
*
     MOVE     "CSVXXX3-INIT-SEC" TO  S-NAME.
*
     MOVE     SPACE          TO   CSV3-REC.
*制御バイト
     MOVE    X"28"           TO   CSV3-F07S  CSV3-F051S
                                  CSV3-F15S
                                  CSV3-F16S.
     MOVE    X"29"           TO   CSV3-F07E  CSV3-F051E
                                  CSV3-F15E
                                  CSV3-F16E.
*カンマ
     MOVE    ","             TO   CSV3-A01
                                  CSV3-A02
                                  CSV3-A03
                                  CSV3-A04
                                  CSV3-A05
                                  CSV3-A06
                                  CSV3-A07
                                  CSV3-A08
                                  CSV3-A09
                                  CSV3-A10
                                  CSV3-A11
                                  CSV3-A12
                                  CSV3-A13
                                  CSV3-A14
                                  CSV3-A15
                                  CSV3-A16
                                  CSV3-A17
                                  CSV3-A18
*#2020/02/25 NAV ST
**********************************CSV3-A19.
                                  CSV3-A051.
*#2020/02/25 NAV ED
*
 CSVXXX3-INIT-EXIT.
     EXIT.
****************************************************************
*ＣＳＶ   SET   3.明細行
****************************************************************
 CSVXXX3-SET-SEC     SECTION.
*
     MOVE     "CSVXXX3-SET-SEC"  TO  S-NAME.
*
*管理番号
     MOVE    SUT-F01         TO   CSV3-F01.
*受信日
     MOVE    SUT-F02         TO   CSV3-F02.
*受信時間
     MOVE    SUT-F03         TO   CSV3-F03.
*取引先ＣＤ
     MOVE    SUT-F04         TO   CSV3-F04.
*作場
     MOVE    SUT-F05         TO   CSV3-F05.
*作場名
     MOVE    SUT-F05         TO   SAK-F01.
     PERFORM SAKUBAL1-READ-SEC.
     IF      SAKUBAL1-INV-FLG = SPACE
             MOVE  SAK-F02        TO   CSV3-F051
     ELSE
             MOVE  ALL NC"？"     TO   CSV3-F051
     END-IF.
*店舗ＣＤ
     MOVE    SUT-F06         TO   CSV3-F06.
*店舗名
     MOVE    SUT-F04         TO   TEN-F01.
     MOVE    SUT-F06         TO   TEN-F02.
     PERFORM NFTENMS1-READ-SEC.
     IF      NFTENMS1-INV-FLG = SPACE
             MOVE  TEN-F04        TO   CSV3-F07
     ELSE
             MOVE  ALL NC"？"     TO   CSV3-F07
     END-IF.
*納品場所
     MOVE    SUT-F07         TO   CSV3-F08.
*県コード
     IF      NFTENMS1-INV-FLG = SPACE
             MOVE    TEN-F12      TO   CSV3-F09
     ELSE
             MOVE    "??"         TO   CSV3-F09
     END-IF.
*出荷日
     MOVE    SUT-F14         TO   CSV3-F10.
*店着日
     MOVE    SUT-F08         TO   CSV3-F11.
*伝票番号
     MOVE    SUT-F09         TO   CSV3-F12.
*ナフコ商品ＣＤ
     MOVE    SUT-F96         TO   CSV3-F13.
*ＪＡＮＣＤ
     MOVE    SUT-F10         TO   CSV3-F14.
*商品名
     MOVE    SUT-F96         TO   SHO-F01.
     PERFORM NFSHOMS1-READ-SEC.
     IF      NFSHOMS1-INV-FLG = SPACE
             MOVE  SHO-F05        TO   CSV3-F15
     ELSE
             MOVE  ALL NC"？"     TO   CSV3-F15
     END-IF.
*規格名
     IF      NFSHOMS1-INV-FLG = SPACE
             MOVE  SHO-F06        TO   CSV3-F16
     ELSE
             MOVE  ALL NC"？"     TO   CSV3-F16
     END-IF.
*納品数
     MOVE    SUT-F11         TO   CSV3-F17.
*発注単位
     IF      NFSHOMS1-INV-FLG = SPACE
             MOVE  SHO-F09        TO   CSV3-F18
*            MOVE  SHO-F09        TO   WK-TANI
*            MOVE  WK-TANI-X      TO   CSV3-F18
     ELSE
             MOVE  ZERO           TO   CSV3-F18
     END-IF.
*原価単価
*#2020/02/25 NAV ST 原価単価削除
*****MOVE    SUT-F89         TO   CSV3-F19.
*#2020/02/25 NAV ED
*    MOVE    SUT-F89         TO   WK-TANKA.
*    MOVE    WK-TANKA-X      TO   CSV3-F19.
*売価単価
     MOVE    SUT-F90         TO   CSV3-F20.
*
 CSVXXX3-SET-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    商品マスタ読込　　                                        *
*--------------------------------------------------------------*
 NFSHOMS1-READ-SEC       SECTION.
*
     READ  NFSHOMS1
           INVALID      MOVE "INV" TO NFSHOMS1-INV-FLG
           NOT INVALID  MOVE SPACE TO NFSHOMS1-INV-FLG
     END-READ.
*
 NFSHOMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    店舗マスタ読込　　                                        *
*--------------------------------------------------------------*
 NFTENMS1-READ-SEC       SECTION.
*
     READ  NFTENMS1
           INVALID      MOVE "INV" TO NFTENMS1-INV-FLG
           NOT INVALID  MOVE SPACE TO NFTENMS1-INV-FLG
     END-READ.
*
 NFTENMS1-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    作場マスタ読込　　                                        *
*--------------------------------------------------------------*
 SAKUBAL1-READ-SEC       SECTION.
*
     READ  SAKUBAL1
           INVALID      MOVE "INV" TO SAKUBAL1-INV-FLG
           NOT INVALID  MOVE SPACE TO SAKUBAL1-INV-FLG
     END-READ.
*
 SAKUBAL1-READ-EXIT.
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
     CLOSE    NFSUTEL1   NFKATCSV NFSHOMS1 NFTENMS1 SAKUBAL1.
*
     DISPLAY  MSG-END UPON CONS.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
