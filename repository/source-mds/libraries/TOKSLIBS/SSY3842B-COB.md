# SSY3842B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3842B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　本部発注業務　　　　　　          *
*    モジュール名　　　　：　本発分更新基本データ作成　　　　　*
*    作成日／更新日　　　：　2015/05/20                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　取込数量ファイルを読み、手書更新  *
*                            基本データを作成する。　　　　　　*
*                            （各マスタを参照すること）        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3842B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          11/12/02.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*取込数量確定ファイル
     SELECT   TRKAKUF   ASSIGN    TO        DA-01-VI-TRKAKUL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       KAK-F10   KAK-F06
                                            KAK-F01   KAK-F02
                                            KAK-F03   KAK-F110
                                            KAK-F111  KAK-F112
                                            KAK-F113  KAK-F11H
                                            KAK-F115  KAK-F116
                                            KAK-F119  KAK-F11A
                                            KAK-F11B
                        FILE  STATUS   IS   KAK-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS   ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       NFM-F01
                        FILE  STATUS   IS   NFM-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01
                                            TBL-F02
                        FILE  STATUS   IS   TBL-STATUS.
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F01
                        FILE      STATUS    MEI-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE      STATUS    TOK-STATUS.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52
                                            TEN-F011
                        FILE      STATUS    TEN-STATUS.
*ナフコ店舗マスタ
     SELECT   NFTENMS   ASSIGN    TO        DA-01-VI-NFTENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       NTE-F01
                                            NTE-F02
                        FILE      STATUS    NTE-STATUS.
*手書伝票基本ファイル
     SELECT   SHTDENWK  ASSIGN    TO        DA-01-S-NFDENWK
                        FILE      STATUS    DEN-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    取込数量確定ファイル
******************************************************************
 FD  TRKAKUF            LABEL RECORD   IS   STANDARD.
     COPY     TRKAKUF   OF        XFDLIB
              JOINING   KAK       PREFIX.
******************************************************************
*    ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS            LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS   OF        XFDLIB
              JOINING   NFM       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
******************************************************************
*    ナフコ店舗マスタ
******************************************************************
 FD  NFTENMS            LABEL RECORD   IS   STANDARD.
     COPY     NFTENMS   OF        XFDLIB
              JOINING   NTE       PREFIX.
******************************************************************
*    手書伝票基本ファイル
******************************************************************
 FD  SHTDENWK           LABEL RECORD   IS   STANDARD.
     COPY     NFDENWK   OF        XFDLIB
              JOINING   DEN       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  DEN-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  ERR-CNT                 PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  ZERO.
 01  NFSHOMS-INV-FLG         PIC  X(03)     VALUE  ZERO.
 01  NFTENMS-INV-FLG         PIC  X(03)     VALUE  ZERO.
*
 01  WRK-AREA.
     03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
     03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  NFM-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  NTE-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3842B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3842B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3842B".
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
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*伝票番号採番サブルーチン
 01  LINK-DENPYO.
     03  LINK-TOKCD         PIC   9(08).
     03  LINK-TENCD         PIC   9(05).
     03  LINK-ERRKBN        PIC   9(01).
     03  LINK-DEN1          PIC   9(09).
     03  LINK-DEN2          PIC   9(09).
*
 LINKAGE                SECTION.
 01  PARA-TRDATE        PIC   9(08).
 01  PARA-TRTIME        PIC   9(06).
 01  PARA-TANCD         PIC   X(02).
 01  PARA-BUMON         PIC   X(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-TRDATE
                                       PARA-TRTIME
                                       PARA-TANCD
                                       PARA-BUMON.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TRKAKUF.
     MOVE      "TRKAKUL2"   TO   AB-FILE.
     MOVE      KAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      NFM-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1 "   TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE      "MEIMS1  "   TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
     MOVE      "TOKMS1  "   TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1  "   TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFTENMS.
     MOVE      "NFTENMS1"   TO   AB-FILE.
     MOVE      NTE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENWK.
     MOVE      "SHTDENWK"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     INITIALIZE                   WRK-AREA.
     OPEN     I-O       TRKAKUF.
     OPEN     INPUT     NFSHOMS  HSHOTBL  HMEIMS
                        HTOKMS   HTENMS   NFTENMS
     OPEN     OUTPUT    SHTDENWK.
*
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
*取込数量確定ファイルスタート
     PERFORM  TRKAKUF-START-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*レコード初期化
     MOVE     SPACE              TO   DEN-REC.
     INITIALIZE                       DEN-REC.
*マスタを索引する。
     PERFORM HSHOTBL-READ-SEC.
     IF  HSHOTBL-INV-FLG = SPACE
         PERFORM HMEIMS-READ-SEC
     END-IF.
     PERFORM HTOKMS-READ-SEC.
     PERFORM HTENMS-READ-SEC.
     PERFORM NFSHOMS-READ-SEC.
     PERFORM NFTENMS-READ-SEC.
*全マスタを索引し、１つでも存在しない場合は取り込まない。
     IF  HSHOTBL-INV-FLG = "INV"
     OR  HMEIMS-INV-FLG  = "INV"
     OR  HTOKMS-INV-FLG  = "INV"
*****OR  HTENMS-INV-FLG  = "INV"
     OR  NFSHOMS-INV-FLG = "INV"
     OR  NFTENMS-INV-FLG = "INV"
         DISPLAY "HSHOTBL-INV-FLG = " HSHOTBL-INV-FLG UPON CONS
         DISPLAY "HMEIMS-INV-FLG  = " HMEIMS-INV-FLG  UPON CONS
         DISPLAY "HTOKMS-INV-FLG  = " HTOKMS-INV-FLG  UPON CONS
         DISPLAY "HTENMS-INV-FLG  = " HTENMS-INV-FLG  UPON CONS
         DISPLAY "NFSHOMS-INV-FLG = " NFSHOMS-INV-FLG UPON CONS
         DISPLAY "NFTENMS-INV-FLG = " NFTENMS-INV-FLG UPON CONS
         ADD    1               TO  ERR-CNT
         GO                     TO  MAIN-010
     END-IF.
*項目セット
     MOVE    KAK-F113           TO  DEN-F01.
     MOVE    KAK-F11A           TO  DEN-F02.
     MOVE    KAK-F11B           TO  DEN-F03.
     MOVE    ZERO               TO  DEN-F04.
     MOVE    40                 TO  DEN-F051.
     MOVE    NC"売上伝票"       TO  DEN-F052.
     MOVE    PARA-TANCD         TO  DEN-F06.
     MOVE    KAK-F115           TO  DEN-F07.
     MOVE    TBL-F04            TO  DEN-F08  DEN-F09.
     MOVE    KAK-F119           TO  DEN-F112.
     MOVE    KAK-F118           TO  DEN-F111.
*出荷日にて０をセット／日次更新処理でセットする。
     MOVE    ZERO               TO  DEN-F113.
     MOVE    KAK-F11G           TO  DEN-F12(1:2).
     MOVE    TBL-F031           TO  DEN-F1411.
     MOVE    TBL-F032           TO  DEN-F1412.
     MOVE    NFM-F07            TO  DEN-F1421.
     MOVE    NFM-F08            TO  DEN-F1422.
     MOVE    KAK-F11E           TO  DEN-F15.
     MOVE    "1"                TO  DEN-F16.
     MOVE    KAK-F11J           TO  DEN-F172.
     MOVE    KAK-F11K           TO  DEN-F173.
     COMPUTE DEN-F181 = DEN-F15 * DEN-F172.
     COMPUTE DEN-F182 = DEN-F15 * DEN-F173.
     MOVE    KAK-F11A           TO  DEN-F23.
     MOVE    TOK-F52            TO  DEN-F24.
     MOVE    KAK-F11C           TO  DEN-F25.
     MOVE    TOK-F89            TO  DEN-F276.
     MOVE    TOK-F89            TO  DEN-F27B.
     MOVE    TEN-F04            TO  DEN-F30.
     MOVE    SYS-DATEW          TO  DEN-F99.
*2013/01/25 NAV ST　支払計上日に管理番号をセット
*****MOVE    KAK-F110           TO  DEN-F68.
     MOVE    KAK-F110           TO  DEN-F93    DEN-F413.
*2013/01/25 NAV ED
     MOVE    KAK-F116           TO  DEN-F94.
     MOVE    KAK-F11G           TO  DEN-F95.
     MOVE    KAK-F11H           TO  DEN-F96.
     MOVE    KAK-F11D           TO  DEN-F97.
*伝票番号採番
     INITIALIZE                     LINK-DENPYO.
     MOVE    DEN-F01            TO  LINK-TOKCD.
     MOVE    DEN-F07            TO  LINK-TENCD.
     CALL   "SKYSBCK3"   USING   LINK-TOKCD  LINK-TENCD
                                 LINK-ERRKBN LINK-DEN1  LINK-DEN2.
     IF  LINK-ERRKBN NOT = ZERO
         DISPLAY "## LINK-ERRKBN = " LINK-ERRKBN UPON CONS
         DISPLAY "## " NC"伝票採番" "ERR]]  "
                 LINK-TOKCD " - " LINK-TENCD  " ##" UPON CONS
         MOVE    4000           TO  PROGRAM-STATUS
         STOP  RUN
     ELSE
         MOVE  LINK-DEN1        TO  DEN-F02  DEN-F23
*20151020MOVE  LINK-DEN1        TO  KAK-F11A
*20151020MOVE  1                TO  KAK-F11B
         MOVE  LINK-DEN1        TO  KAK-F12
     END-IF.
*
     WRITE   DEN-REC.
     ADD     1                  TO  DEN-ADD-CNT.
*
     REWRITE KAK-REC.
*
 MAIN-010.
     PERFORM  TRKAKUF-READ-SEC.
*
     IF  SUTE-FLG = "END"
         MOVE       "END"        TO   END-FLG
     END-IF.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　取込数量確定ファイル　スタート
****************************************************************
 TRKAKUF-START-SEC     SECTION.
*
     MOVE    "TRKAKUF-READ-SEC"  TO   S-NAME.
*
     MOVE     SPACE              TO   KAK-REC.
     INITIALIZE                       KAK-REC.
*
     MOVE     SPACE              TO   KAK-F10.
     MOVE     SPACE              TO   KAK-F06.
     MOVE     "2"                TO   KAK-F01.
     MOVE     PARA-TRDATE        TO   KAK-F02.
     MOVE     PARA-TRTIME        TO   KAK-F03.
*
     START  TRKAKUF  KEY  IS  >=  KAK-F10  KAK-F06  KAK-F01
                                  KAK-F02  KAK-F03  KAK-F110
                                  KAK-F111 KAK-F112 KAK-F113
                                  KAK-F11H KAK-F115 KAK-F116
                                  KAK-F119 KAK-F11A KAK-F11B
           INVALID
           MOVE  "END"           TO      END-FLG
           DISPLAY "＃処理対象がありません１＃" UPON CONS
           GO                    TO      TRKAKUF-START-EXIT
     END-START.
*取込数量確定ファイル読込
     PERFORM  TRKAKUF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY "＃処理対象がありません２＃" UPON CONS
     END-IF.
*
 TRKAKUF-START-EXIT.
     EXIT.
****************************************************************
*　　　　　　箱数ファイル出力                                  *
****************************************************************
 TRKAKUF-READ-SEC            SECTION.
*
     MOVE    "TRKAKUF-READ-SEC" TO        S-NAME.
*
     READ  TRKAKUF  AT  END
           MOVE  "END"          TO        SUTE-FLG
           GO                   TO        TRKAKUF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                   TO   TRKAKUF-READ-CNT.
 READ010.
*更新区分チェック
     IF       KAK-F10  =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRKAKUF-READ-EXIT
     END-IF.
 READ020.
*エラー区分チェック
     IF       KAK-F06  =  "1"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRKAKUF-READ-EXIT
     END-IF.
 READ030.
*オンライン／手書区分チェック
     IF       KAK-F01  NOT =  "2"
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRKAKUF-READ-EXIT
     END-IF.
 READ040.
*取込日付チェック
     IF       KAK-F02  NOT =  PARA-TRDATE
     OR       KAK-F03  NOT =  PARA-TRTIME
              MOVE     "END"      TO   END-FLG
              GO                  TO   TRKAKUF-READ-EXIT
     END-IF.
*
 TRKAKUF-READ-EXIT.
     EXIT.
****************************************************************
*                商品変換テーブル読込み                        *
****************************************************************
 HSHOTBL-READ-SEC          SECTION.
*
     MOVE      "HSHOTBL-READ-SEC" TO    S-NAME.
*
     MOVE      KAK-F113    TO     TBL-F01.
     MOVE      KAK-F11C    TO     TBL-F02.
     READ      HSHOTBL
               INVALID
               MOVE      "INV"    TO    HSHOTBL-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*                商品名称マスタ読込み                          *
****************************************************************
 HMEIMS-READ-SEC           SECTION.
*
     MOVE      "HMEIMS-READ-SEC"  TO    S-NAME.
     MOVE      SPACE       TO     HMEIMS-INV-FLG.
*
     MOVE      TBL-F031    TO     MEI-F011.
     MOVE      TBL-F032    TO     MEI-F012.
     READ      HMEIMS
               INVALID
               MOVE      "INV"    TO    HMEIMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HMEIMS-INV-FLG
     END-READ.
*
 HMEIMS-READ-EXIT.
     EXIT.
****************************************************************
*                取引先マスタ　　　　　                        *
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE      "HTOKMS-READ-SEC"  TO    S-NAME.
*
     MOVE      KAK-F113    TO     TOK-F01.
     READ      HTOKMS
               INVALID
               MOVE      "INV"    TO    HTOKMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*                店舗マスタ読込　　　　                        *
****************************************************************
 HTENMS-READ-SEC           SECTION.
*
     MOVE      "HTENMS-READ-SEC"  TO    S-NAME.
*
     MOVE      KAK-F113    TO     TEN-F52.
     MOVE      KAK-F115    TO     TEN-F011.
     READ      HTENMS
               INVALID
               MOVE      "INV"    TO    HTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*                ナフコ商品マスタ読込　                        *
****************************************************************
 NFSHOMS-READ-SEC          SECTION.
*
     MOVE      "NFSHOMS-READ-SEC" TO    S-NAME.
*
     MOVE      KAK-F11C    TO     NFM-F01.
     READ      NFSHOMS
               INVALID
               MOVE      "INV"    TO    NFSHOMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    NFSHOMS-INV-FLG
     END-READ.
*
 NFSHOMS-INV-EXIT.
     EXIT.
****************************************************************
*          ナフコ店舗マスタ読込　　　　                        *
****************************************************************
 NFTENMS-READ-SEC          SECTION.
*
     MOVE      "NFTENMS-READ-SEC" TO    S-NAME.
*
     MOVE      KAK-F113    TO     NTE-F01.
     MOVE      KAK-F115    TO     NTE-F02.
     READ      NFTENMS
               INVALID
               MOVE      "INV"    TO    NFTENMS-INV-FLG
               NOT  INVALID
               MOVE      SPACE    TO    NFTENMS-INV-FLG
     END-READ.
*
 NFTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*取込数量確定ファイル読込
     DISPLAY "TRKAKUF   READ CNT = " TRKAKUF-READ-CNT UPON CONS.
*取込エラーカウント
     DISPLAY "ERROR          CNT = " ERR-CNT          UPON CONS.
*手書伝票基本データ
     DISPLAY "SHTDENWK  WRT  CNT = " DEN-ADD-CNT      UPON CONS.
*
     CLOSE     TRKAKUF  NFSHOMS  HSHOTBL  HMEIMS  HTOKMS
               HTENMS   SHTDENWK NFTENMS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
