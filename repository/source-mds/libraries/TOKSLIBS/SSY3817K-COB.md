# SSY3817K

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3817K.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷支援システム　　　　　　*
*    業務名　　　　　　　：　発注業務　　　　　　　            *
*    モジュール名　　　　：　発注関係資料データ抽出　　　　　　*
*    作成日／更新日　　　：　2015/05/11                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　基本情報ファイルより、　　　　　  *
*                            数量／箱数確定データをそれぞれ　　*
*                            のファイルに出力する。            *
*    作成日／更新日　　　：　2015/08/13                        *
*                            数量訂正ファイル出荷日への        *
*                            転送仕様変更　　　　　　　        *
*    作成日／更新日　　　：　2015/10/01-05                     *
*                            １．Ｅ１の梱包数＝１固定　　　    *
*                            ２．箱数計算方法を変更　　        *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY3817K.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/05/11.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*基本情報ファイル
     SELECT   NFJOHOL1  ASSIGN    TO        DA-01-VI-NFJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-F01
                                            JOH-F05
                                            JOH-F06
                                            JOH-F07
                                            JOH-F08
                                            JOH-F09
                        FILE  STATUS   IS   JOH-STATUS.
*数量訂正ファイル
     SELECT   NFSUTEL1  ASSIGN    TO        DA-01-VI-NFSUTEL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       STE-F01
                                            STE-F05
                                            STE-F06
                                            STE-F07
                                            STE-F08
                                            STE-F09
                                            WITH DUPLICATES
                        FILE  STATUS   IS   STE-STATUS.
*箱数ファイル
     SELECT   NFHAKOL1  ASSIGN    TO        DA-01-VI-NFHAKOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       HAK-F01
                                            HAK-F05
                                            HAK-F06
                                            HAK-F07
                                            HAK-F08
                                            WITH DUPLICATES
                        FILE STATUS    IS   HAK-STATUS.
*ナフコ商品マスタ
     SELECT   NFSHOMS1  ASSIGN    TO        DA-01-VI-NFSHOMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SHO-F01
                        FILE STATUS    IS   SHO-STATUS.
*総梱包数計算ファイル
     SELECT   HAKOXXX1  ASSIGN    TO        DA-01-VI-HAKOXXX1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HKK-F01
                                            HKK-F05
                                            HKK-F06
                                            HKK-F07
                                            HKK-F08
                        FILE STATUS    IS   HKK-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    基本情報ファイル
******************************************************************
 FD  NFJOHOL1           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOL1  OF        XFDLIB
              JOINING   JOH       PREFIX.
******************************************************************
*    数量訂正ファイル　　　
******************************************************************
 FD  NFSUTEL1           LABEL RECORD   IS   STANDARD.
     COPY     NFSUTEL1  OF        XFDLIB
              JOINING   STE       PREFIX.
******************************************************************
*    箱数ファイル
******************************************************************
 FD  NFHAKOL1           LABEL RECORD   IS   STANDARD.
     COPY     NFHAKOL1  OF        XFDLIB
              JOINING   HAK  AS   PREFIX.
******************************************************************
*    ナフコ商品マスタ
******************************************************************
 FD  NFSHOMS1           LABEL RECORD   IS   STANDARD.
     COPY     NFSHOMS1  OF        XFDLIB
              JOINING   SHO  AS   PREFIX.
******************************************************************
*    総梱包数計算ファイル
******************************************************************
 FD  HAKOXXX1           LABEL RECORD   IS   STANDARD.
     COPY     HAKOXXXF  OF        XFDLIB
              JOINING   HKK  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  SUTE-FLG                PIC  X(03)     VALUE  ZERO.
 01  DEL-FLG                 PIC  X(03)     VALUE  ZERO.
 01  KEP-FLG                 PIC  X(01)     VALUE  ZERO.
 01  SKIP-CNT                PIC  9(07)     VALUE  ZERO.
 01  NFJOHOL1-READ-CNT       PIC  9(07)     VALUE  ZERO.
 01  TRKAKUF-READ-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFJOHOL1-SKIP-CNT       PIC  9(07)     VALUE  ZERO.
 01  HAK-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  HAK-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  STE-DATA-CNT            PIC  9(07)     VALUE  ZERO.
 01  STE-GYO-CNT             PIC  9(07)     VALUE  ZERO.
 01  STE-WT-CNT              PIC  9(07)     VALUE  ZERO.
 01  STE-REWT-CNT            PIC  9(07)     VALUE  ZERO.
 01  KIHON-ADD-CNT           PIC  9(07)     VALUE  ZERO.
 01  KIHON-UPD-CNT           PIC  9(07)     VALUE  ZERO.
 01  DEN-UPD-CNT             PIC  9(07)     VALUE  ZERO.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  ZAMZAIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  NFHAKOL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFHAKOL1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFHAKOL1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFSUTEL1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFSUTEL1-END-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFSUTEL1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFKOSUL1-DEL-CNT        PIC  9(07)     VALUE  ZERO.
 01  NFSHOMS1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  NFJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HAKOXXX1-INV-FLG        PIC  X(03)     VALUE  SPACE.
 01  WK-SHO-F10              PIC  9(06)     VALUE  ZERO.
*
 01  WRK-AREA.
     03  WRK-TEISUU          PIC S9(09)V99  VALUE  ZERO.
     03  WRK-MAETEISUU       PIC S9(09)V99  VALUE  ZERO.
*計算領域
 01  WRK-AREA2.
     03  WRK-HIK             PIC S9(09)V9(02)  VALUE ZERO.
     03  WRK-ZAI             PIC S9(09)V9(02)  VALUE ZERO.
*計算領域
 01  WRK-AREA3.
     03  WK-TORAY0.
       05  WK-TORAY          PIC 9(06)V9(02)   VALUE ZERO.
     03  WK-TORAYR           REDEFINES WK-TORAY0.
       05  WK-TORAY1         PIC 9(08).
     03  WK-KONPO0.
       05  WK-KONPO          PIC 9(06)V9(01)    VALUE ZERO.
     03  WK-KONPOR           REDEFINES WK-KONPO0.
       05  WK-KONPO1         PIC 9(07).
*総梱包数計算エリア
 01  KEI-KONPOU.
     03  KEI-TORAY-1.
         05  KEI-TORAY-1-1 PIC 9(06)V9(01)    VALUE ZERO.
         05  KEI-TORAY-1-2 PIC 9(06)V9(01)    VALUE ZERO.
     03  KEI-TORAY-2.
         05  KEI-TORAY-2-1 PIC 9(06)V9(01)    VALUE ZERO.
         05  KEI-TORAY-2-2 PIC 9(06)V9(01)    VALUE ZERO.
     03  KEI-TORAY-3.
         05  KEI-TORAY-3-1 PIC 9(06)V9(01)    VALUE ZERO.
         05  KEI-TORAY-3-2 PIC 9(06)V9(01)    VALUE ZERO.
     03  KEI-TORAY-4.
         05  KEI-TORAY-4-1 PIC 9(06)V9(01)    VALUE ZERO.
         05  KEI-TORAY-4-2 PIC 9(06)V9(01)    VALUE ZERO.
     03  KEI-TORAY-5.
         05  KEI-TORAY-5-1 PIC 9(06)V9(01)    VALUE ZERO.
         05  KEI-TORAY-5-2 PIC 9(06)V9(01)    VALUE ZERO.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  KAK-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  DEN-STATUS        PIC  X(02).
     03  ZAI-STATUS        PIC  X(02).
     03  STE-STATUS        PIC  X(02).
     03  HAK-STATUS        PIC  X(02).
     03  SHO-STATUS        PIC  X(02).
     03  HKK-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3817K".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3817K".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY3817K".
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
*
 LINKAGE                SECTION.
 01  PARA-IN-KANRINO    PIC   9(08).
 01  PARA-IN-SAKUBACD   PIC   X(02).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-KANRINO
                                       PARA-IN-SAKUBACD.
 DECLARATIVES.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFJOHOL1.
     MOVE      "NFJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSUTEL1.
     MOVE      "NFSUTEL1"   TO   AB-FILE.
     MOVE      STE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFHAKOL1.
     MOVE      "NFHAKOL1"   TO   AB-FILE.
     MOVE      HAK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   NFSHOMS1.
     MOVE      "NFSHOMS1"   TO   AB-FILE.
     MOVE      SHO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HAKOXXX1.
     MOVE      "HAKOXXX1"   TO   AB-FILE.
     MOVE      HKK-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     INITIALIZE                   WRK-AREA.
     INITIALIZE                   NFSUTEL1-DEL-CNT.
     INITIALIZE                   NFKOSUL1-DEL-CNT.
*
     OPEN     INPUT     NFJOHOL1  NFSHOMS1.
     OPEN     I-O       NFSUTEL1  NFHAKOL1.
     OPEN     I-O       HAKOXXX1.
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
*
 INIT-01.
*基本情報ファイルスタート
     PERFORM  NFJOHOL1-START-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　取込対象データ無１　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-02.
*基本情報ファイル読込
     PERFORM NFJOHOL1-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　取込対象データ無２　＃＃"  UPON CONS
          MOVE "END"            TO   END-FLG
          MOVE 4010             TO   PROGRAM-STATUS
          GO                    TO   INIT-EXIT
     END-IF.
*
 INIT-03.
*数量訂正ファイル事前削除（対象ＫＥＹのみ）
     PERFORM NFSUTEL1-START-SEC.
     IF      NFSUTEL1-INV-FLG = "INV"
             DISPLAY NC"数量訂正ファイル削除＝"
                     NFSUTEL1-DEL-CNT NC"件"     UPON CONS
             GO       TO         INIT-04
     END-IF.
*
     PERFORM NFSUTEL1-DEL-SEC    UNTIL NFSUTEL1-END-FLG = "END".
     DISPLAY NC"数量訂正ファイル削除＝"
                     NFSUTEL1-DEL-CNT NC"件"  UPON CONS.
*
 INIT-04.
*箱数ファイル事前削除（対象ＫＥＹのみ）
     PERFORM NFHAKOL1-START-SEC.
     IF      NFHAKOL1-INV-FLG = "INV"
             DISPLAY NC"箱数ファイル削除＝"
                     NFHAKOL1-DEL-CNT NC"件"     UPON CONS
             GO       TO         INIT-05
     END-IF.
*
     PERFORM NFHAKOL1-DEL-SEC    UNTIL NFHAKOL1-END-FLG = "END".
     DISPLAY NC"箱数ファイル削除＝"
                     NFHAKOL1-DEL-CNT NC"件" UPON CONS.
*
 INIT-05.
     CLOSE              NFSUTEL1  NFHAKOL1.
     OPEN     I-O       NFSUTEL1  NFHAKOL1.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    基本情報ファイルスタート
****************************************************************
 NFJOHOL1-START-SEC          SECTION.
*
     MOVE    "NFJOHOL1-START-SEC"  TO   S-NAME.
*
     MOVE     SPACE               TO   JOH-REC.
     INITIALIZE                        JOH-REC.
*
     MOVE     PARA-IN-KANRINO     TO   JOH-F01.
     MOVE     PARA-IN-SAKUBACD    TO   JOH-F05.
*
     START  NFJOHOL1  KEY  IS  >= JOH-F01
                                  JOH-F05
                                  JOH-F06
                                  JOH-F07
                                  JOH-F08
                                  JOH-F09
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 NFJOHOL1-START-EXIT.
     EXIT.
*
****************************************************************
*    基本情報ファイル読込
****************************************************************
 NFJOHOL1-READ-SEC           SECTION.
*
     MOVE    "NFJOHOL1-READ-SEC"   TO   S-NAME.
*
     READ     NFJOHOL1  AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFJOHOL1-READ-EXIT
     END-READ.
*発注確定区分チェック
     IF       JOH-F34  =  "1"
              ADD          1      TO   NFJOHOL1-READ-CNT
              ADD          1      TO   NFJOHOL1-SKIP-CNT
              GO                  TO   NFJOHOL1-READ-SEC
     END-IF.
*管理番号チェック
     IF       JOH-F01  NOT =  PARA-IN-KANRINO
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFJOHOL1-READ-EXIT
     END-IF.
*作場チェック
     IF       PARA-IN-SAKUBACD  NOT =  "  "
        IF    JOH-F05  NOT =  PARA-IN-SAKUBACD
              MOVE     "END"      TO   END-FLG
              GO                  TO   NFJOHOL1-READ-EXIT
     END-IF.
*
     ADD      1                   TO   NFJOHOL1-READ-CNT.
*
 NFJOHOL1-READ-EXIT.
     EXIT.
*
****************************************************************
*    数量訂正ファイルＳＴＡＲＴ
****************************************************************
 NFSUTEL1-START-SEC            SECTION.
*
     MOVE    "NFSUTEL1-START-SEC" TO   S-NAME.
*
 NFSUTEL1-STRAT-01.
*
     MOVE     SPACE               TO   STE-REC.
     INITIALIZE                        STE-REC.
*
     MOVE     PARA-IN-KANRINO     TO   STE-F01.
     MOVE     PARA-IN-SAKUBACD    TO   STE-F05.
*
     START    NFSUTEL1  KEY  IS   >=   STE-F01
                                       STE-F05
                                       STE-F06
                                       STE-F07
                                       STE-F08
                                       STE-F09
              INVALID
              MOVE    "INV"       TO   NFSUTEL1-INV-FLG
              GO                  TO   NFSUTEL1-START-EXIT
              NOT INVALID
              MOVE    "   "       TO   NFSUTEL1-INV-FLG
     END-START.
*
 NFSUTEL1-START-EXIT.
     EXIT.
*
****************************************************************
*    数量訂正ファイル事前削除（対象ＫＥＹのみ）
****************************************************************
 NFSUTEL1-DEL-SEC            SECTION.
*
     MOVE    "NFSUTEL1-DEL-SEC  " TO   S-NAME.
*
 NFSUTEL1-DEL-01.
*
     READ     NFSUTEL1  NEXT  AT  END
              MOVE     "END"      TO   NFSUTEL1-END-FLG
              GO                  TO   NFSUTEL1-DEL-EXIT
     END-READ.
*
 NFSUTEL1-DEL-02.
*
*発注確定区分チェック
     IF       STE-F91  =  "1"
              GO                  TO   NFSUTEL1-DEL-01
     END-IF.
*管理番号チェック
     IF       STE-F01  NOT =  PARA-IN-KANRINO
              MOVE     "END"      TO   NFSUTEL1-END-FLG
              GO                  TO   NFSUTEL1-DEL-EXIT
     END-IF.
*作場チェック
     IF       PARA-IN-SAKUBACD  NOT =  "  "
        IF    STE-F05  NOT =  PARA-IN-SAKUBACD
              MOVE     "END"      TO   NFSUTEL1-END-FLG
              GO                  TO   NFSUTEL1-DEL-EXIT
     END-IF.
*
 NFSUTEL1-DEL-03.
*
     DELETE   NFSUTEL1.
     ADD      1        TO         NFSUTEL1-DEL-CNT.
*
 NFSUTEL1-DEL-EXIT.
     EXIT.
*
****************************************************************
*    箱数ファイルＳＴＡＲＴ
****************************************************************
 NFHAKOL1-START-SEC            SECTION.
*
     MOVE    "NFHAKOL1-START-SEC" TO   S-NAME.
*
 NFHAKOL1-STRAT-01.
*
     MOVE     SPACE               TO   HAK-REC.
     INITIALIZE                        HAK-REC.
*
     MOVE     PARA-IN-KANRINO     TO   HAK-F01.
     MOVE     PARA-IN-SAKUBACD    TO   HAK-F05.
*
     START    NFHAKOL1  KEY  IS   >=   HAK-F01
                                       HAK-F05
                                       HAK-F06
                                       HAK-F07
                                       HAK-F08
              INVALID
              MOVE    "INV"       TO   NFHAKOL1-INV-FLG
              GO                  TO   NFHAKOL1-START-EXIT
              NOT INVALID
              MOVE    "   "       TO   NFHAKOL1-INV-FLG
     END-START.
*
 NFHAKOL1-START-EXIT.
     EXIT.
*
****************************************************************
*    箱数ファイル事前削除（対象ＫＥＹのみ）
****************************************************************
 NFHAKOL1-DEL-SEC            SECTION.
*
     MOVE    "NFHAKOL1-DEL-SEC  " TO   S-NAME.
*
 NFHAKOL1-DEL-01.
*
     READ     NFHAKOL1  NEXT  AT  END
              MOVE     "END"      TO   NFHAKOL1-END-FLG
              GO                  TO   NFHAKOL1-DEL-EXIT
     END-READ.
*
 NFHAKOL1-DEL-02.
*
*発注確定区分チェック
     IF       HAK-F93  =  "1"
              GO                  TO   NFHAKOL1-DEL-01
     END-IF.
*管理番号チェック
     IF       HAK-F01  NOT =  PARA-IN-KANRINO
              MOVE     "END"      TO   NFHAKOL1-END-FLG
              GO                  TO   NFHAKOL1-DEL-EXIT
     END-IF.
*作場チェック
     IF       PARA-IN-SAKUBACD  NOT =  "  "
        IF    HAK-F05  NOT =  PARA-IN-SAKUBACD
              MOVE     "END"      TO   NFHAKOL1-END-FLG
              GO                  TO   NFHAKOL1-DEL-EXIT
     END-IF.
*
 NFHAKOL1-DEL-03.
*
     DELETE   NFHAKOL1.
     ADD      1        TO         NFHAKOL1-DEL-CNT.
*
 NFHAKOL1-DEL-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
*商品名称マスタ存在チェック（入数取得）
 MAIN-000.
     PERFORM NFSHOMS1-READ-SEC.
     IF      NFSHOMS1-INV-FLG = "INV"
             DISPLAY  NC"ナフコ商品マスタなし　商品Ｃ＝"
                      JOH-F13     UPON  CONS
             ADD  1   TO         NFJOHOL1-SKIP-CNT
             GO       TO         MAIN-999
     END-IF.
     IF      SHO-F09          =  ZERO
             DISPLAY  NC"ナフコ商品マスタ入数ゼロ！　商品Ｃ＝"
                      JOH-F13     UPON  CONS
             ADD  1   TO         NFJOHOL1-SKIP-CNT
             GO       TO         MAIN-999
     END-IF.
*トレー数算出
 MAIN-001.
     IF      NFSHOMS1-INV-FLG = "   "
             COMPUTE  WK-TORAY  =  JOH-F20 / SHO-F09
             IF       WK-TORAY1(8:1)  NOT = 0
                      ADD   0.1    TO  WK-TORAY
             END-IF
     ELSE
             GO       TO           MAIN-999
     END-IF.
*数量訂正ファイル出力
 MAIN-010.
     PERFORM NFSUTEL1-WT-SEC.
*
*箱数ファイル存在チェック
 MAIN-020.
     PERFORM  NFHAKOL1-READ-SEC.
*****発注確定済は更新しない。
     IF   NFHAKOL1-INV-FLG = SPACE
          IF  HAK-F93 = "1"
              GO                 TO   MAIN-999
          END-IF
     END-IF.
*箱数確定ファイル出力（明細が出力ない場合は箱数Ｆは作成しない）
     IF  STE-GYO-CNT > ZERO
         IF  NFHAKOL1-INV-FLG = "INV"
             MOVE  SPACE        TO  HAK-REC
             INITIALIZE             HAK-REC
             MOVE  JOH-F01      TO  HAK-F01
             MOVE  JOH-F02      TO  HAK-F02
             MOVE  JOH-F03      TO  HAK-F03
             MOVE  JOH-F04      TO  HAK-F04
             MOVE  JOH-F05      TO  HAK-F05
             MOVE  JOH-F06      TO  HAK-F06
             MOVE  JOH-HE20     TO  HAK-F07
             MOVE  JOH-F09      TO  HAK-F08
             MOVE  1            TO  HAK-F10
             MOVE  WK-TORAY     TO  HAK-F11
*総梱数算出
             IF    NFSHOMS1-INV-FLG = "   "
                   COMPUTE  WK-KONPO  =  HAK-F11  / 2
                   IF       WK-KONPO1(7:1)  NOT = 0
                            ADD   1      TO  WK-KONPO
                   END-IF
             ELSE
                   GO       TO           MAIN-999
             END-IF
*
*************総梱包数計算
             PERFORM  NEWKONPOU-KEISAN-SEC
             MOVE  WK-KONPO     TO  HAK-F09
*************倉庫ＣＤがＥ１（甘木）の場合は、総梱包数に１セット
             IF    HAK-F05  =  "E1"  OR  "AL"  OR  "AA"
                   MOVE   1     TO  HAK-F09
             END-IF
             WRITE HAK-REC
             ADD   1            TO  HAK-ADD-CNT
         ELSE
             MOVE  JOH-F02      TO  HAK-F02
             MOVE  JOH-F03      TO  HAK-F03
             MOVE  JOH-F04      TO  HAK-F04
             ADD   1            TO  HAK-F10
             ADD   WK-TORAY     TO  HAK-F11
*総梱数算出
             IF    NFSHOMS1-INV-FLG = "   "
                   COMPUTE  WK-KONPO  =  HAK-F11  / 2
                   IF       WK-KONPO1(7:1)  NOT = 0
                            ADD   1      TO  WK-KONPO
                   END-IF
             ELSE
                   GO       TO           MAIN-999
             END-IF
*
*************総梱包数計算
             PERFORM  NEWKONPOU-KEISAN-SEC
             MOVE  WK-KONPO     TO  HAK-F09
*************倉庫ＣＤがＥ１（甘木）の場合は、総梱包数に１セット
             IF    HAK-F05  =  "E1"  OR  "AL"  OR  "AA"
                   MOVE   1     TO  HAK-F09
             END-IF
             REWRITE HAK-REC
             ADD   1            TO  HAK-UPD-CNT
         END-IF
     END-IF.
*
 MAIN-999.
     PERFORM  NFJOHOL1-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ商品マスタ読込（存在チェック）
****************************************************************
 NFSHOMS1-READ-SEC           SECTION.
*
     MOVE    "NFSHOMS1-READ-SEC"  TO   S-NAME.
*
     MOVE     JOH-F13             TO   SHO-F01.     *>相手商品
     READ     NFSHOMS1
              INVALID     MOVE  "INV"  TO   NFSHOMS1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFSHOMS1-INV-FLG
     END-READ.
*
 NFSHOMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　数量訂正ファイル出力                              *
****************************************************************
 NFSUTEL1-WT-SEC              SECTION.
*
     MOVE    "NFSUTEL1-WT-SEC"   TO        S-NAME.
*
     MOVE  SPACE        TO        STE-REC.
     INITIALIZE                   STE-REC.
*管理番号
     MOVE  JOH-F01      TO        STE-F01.
*バッチ日付
     MOVE  JOH-F02      TO        STE-F02.
*バッチ時間
     MOVE  JOH-F03      TO        STE-F03.
*バッチ取引先
     MOVE  JOH-F04      TO        STE-F04.
*作場ＣＤ
     MOVE  JOH-F05      TO        STE-F05.
*店舗ＣＤ
     MOVE  JOH-F06      TO        STE-F06.
*納品場所
     MOVE  JOH-HE20     TO        STE-F07.
*店着日
     MOVE  JOH-F09      TO        STE-F08.
*伝票番号
     MOVE  JOH-F07      TO        STE-F09.
*商品ＣＤ
     MOVE  JOH-ME04     TO        STE-F10.
*納品数
     MOVE  JOH-F20      TO        STE-F11.
*訂正区分
     IF    JOH-F19  NOT =    JOH-F20
           MOVE "04"    TO        STE-F12
     ELSE
           MOVE "  "    TO        STE-F12
     END-IF.
*入荷予定日
     MOVE  JOH-F11      TO        STE-F13.
*出荷日
* 基本情報ファイルの出荷日≠の場合は出荷日
     IF    JOH-F10 NOT = ZERO
           MOVE  JOH-F10     TO   STE-F14
* 出荷日＝0の場合は納品日の１日前
     ELSE
           MOVE    "6"             TO   LINK-IN-KBN
           MOVE     1              TO   LINK-IN-YMD6
           MOVE     JOH-F09        TO   LINK-IN-YMD8
           CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                        LINK-IN-YMD6
                                        LINK-IN-YMD8
                                        LINK-OUT-RET
                                        LINK-OUT-YMD8
           IF       LINK-OUT-RET   =    ZERO
                    MOVE    LINK-OUT-YMD8    TO   STE-F14
           ELSE
                    DISPLAY NC"出荷日算出エラー！"     UPON CONS
                    DISPLAY NC"算出元納品日＝" JOH-F09 UPON CONS
                    DISPLAY NC"エラーＣＤ　＝" LINK-OUT-RET
                                                       UPON CONS
                    MOVE    4010             TO   PROGRAM-STATUS
                    STOP    RUN
     END-IF.
*発注形態
     IF    JOH-F01(1:1) =         9
           MOVE "2"     TO        STE-F15
           MOVE  "??"   TO        STE-FIL(1:2)
     ELSE
           MOVE "1"     TO        STE-F15
           MOVE  "00"   TO        STE-FIL(1:2)
     END-IF.
*発注形態セット
     MOVE  JOH-HE09     TO        STE-F16.
     MOVE  JOH-HE09     TO        STE-FIL(1:2).
*原価／売価セット
     MOVE  JOH-ME10     TO        STE-F89.
     MOVE  JOH-ME12     TO        STE-F90.
*インストアＣＤ
     MOVE  JOH-ME17     TO        STE-F96.
     MOVE  "3"          TO        LINK-IN-KBN.
     MOVE  JOH-HE12     TO        LINK-IN-YMD6.
     CALL  "SKYDTCKB"   USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
*指定納品日
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   STE-F97
     ELSE
         MOVE    ZERO           TO   STE-F97
     END-IF.
*
     WRITE STE-REC.
*
     ADD   1            TO        STE-DATA-CNT.
     ADD   1            TO        STE-GYO-CNT.
     ADD   1            TO        STE-WT-CNT.
*
 NFSUTEL1-WT-EXIT.
     EXIT.
****************************************************************
*    箱数確定ファイル読込（存在チェック）
****************************************************************
 NFHAKOL1-READ-SEC           SECTION.
*
     MOVE    "NFHAKOL1-READ-SEC"  TO   S-NAME.
*
     MOVE     JOH-F01             TO   HAK-F01.     *>管理番号
     MOVE     JOH-F05             TO   HAK-F05.     *>作場ＣＤ
     MOVE     JOH-F06             TO   HAK-F06.     *>店舗ＣＤ
     MOVE     JOH-HE20            TO   HAK-F07.     *>納品場所
     MOVE     JOH-F09             TO   HAK-F08.     *>店着日
     READ     NFHAKOL1
              INVALID     MOVE  "INV"  TO   NFHAKOL1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   NFHAKOL1-INV-FLG
     END-READ.
*
 NFHAKOL1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*基本情報ファイル読込
     DISPLAY "NFJOHOL1  READ CNT = " NFJOHOL1-READ-CNT UPON CONS.
*取込数量確定ファイル読込
*    DISPLAY "TRKAKUF   READ CNT = " TRKAKUF-READ-CNT UPON CONS.
*ＳＫＩＰ
     DISPLAY "NFJOHOL1  SKIP CNT = " NFJOHOL1-SKIP-CNT UPON CONS.
*数量訂正ファイル追加
     DISPLAY "NFSUTEL1  WRT  CNT = " STE-WT-CNT       UPON CONS.
*数量訂正ファイル更新
*    DISPLAY "NFSUTEL1  UPD  CNT = " STE-REWT-CNT     UPON CONS.
*箱数ファイル追加
     DISPLAY "NFHAKOL1  WRT  CNT = " HAK-ADD-CNT      UPON CONS.
*箱数ファイル更新
     DISPLAY "NFHAKOL1  UPD  CNT = " HAK-UPD-CNT      UPON CONS.
*
     CLOSE     NFJOHOL1  NFSHOMS1
               NFSUTEL1  NFHAKOL1.
     CLOSE     HAKOXXX1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　総梱包数計算ファイル編集
****************************************************************
 NEWKONPOU-KEISAN-SEC       SECTION.
*
     MOVE "NEWKONPOU-KEISAN-SEC" TO S-NAME.
*
     MOVE     JOH-F01             TO   HKK-F01.     *>管理番号
     MOVE     JOH-F05             TO   HKK-F05.     *>作場ＣＤ
     MOVE     JOH-F06             TO   HKK-F06.     *>店舗ＣＤ
     MOVE     JOH-HE20            TO   HKK-F07.     *>納品場所
     MOVE     JOH-F09             TO   HKK-F08.     *>店着日
**       DISPLAY "HKK-F01       = " HKK-F01       UPON CONS.
**       DISPLAY "HKK-F05       = " HKK-F05       UPON CONS.
**       DISPLAY "HKK-F06       = " HKK-F06       UPON CONS.
**       DISPLAY "HKK-F07       = " HKK-F07       UPON CONS.
**       DISPLAY "HKK-F08       = " HKK-F08       UPON CONS.
     READ     HAKOXXX1
              INVALID     MOVE  "INV"  TO   HAKOXXX1-INV-FLG
              NOT INVALID MOVE  SPACE  TO   HAKOXXX1-INV-FLG
     END-READ.
*
     IF  NFHAKOL1-INV-FLG = "INV"
         MOVE  SPACE        TO  HKK-REC
         INITIALIZE             HKK-REC
         MOVE  JOH-F01      TO  HKK-F01
         MOVE  JOH-F02      TO  HKK-F02
         MOVE  JOH-F03      TO  HKK-F03
         MOVE  JOH-F04      TO  HKK-F04
         MOVE  JOH-F05      TO  HKK-F05
         MOVE  JOH-F06      TO  HKK-F06
         MOVE  JOH-HE20     TO  HKK-F07
         MOVE  JOH-F09      TO  HKK-F08
**        DISPLAY "JOH-F09 = " JOH-F09 UPON CONS
**        DISPLAY "HKK-F080= " HKK-F08 UPON CONS
*##      MOVE  WK-TORAY     TO  HAK-F11
*総梱数算出
         IF    NFSHOMS1-INV-FLG = "   "
               MOVE  SHO-F10 TO WK-SHO-F10
**             DISPLAY "WK-SHO-F10 = " WK-SHO-F10 UPON CONS
               MOVE ZERO TO HKK-F11 HKK-F12 HKK-F13 HKK-F14
               EVALUATE  WK-SHO-F10
                   WHEN  1   MOVE  WK-TORAY TO HKK-F11
                   WHEN  2   MOVE  WK-TORAY TO HKK-F12
                   WHEN  3   MOVE  WK-TORAY TO HKK-F13
                   WHEN  4   MOVE  WK-TORAY TO HKK-F14
                   WHEN OTHER MOVE WK-TORAY TO HKK-F11
                        DISPLAY NC"＃箱数算出値取得エラー＃"
                        UPON CONS
               END-EVALUATE
*##      ELSE
*##            MOVE  WK-TORAY               TO HKK-F11
         END-IF
*********総梱包数計算
         MOVE  HKK-F11       TO   KEI-TORAY-1-1
         COMPUTE  KEI-TORAY-1-2 = KEI-TORAY-1-1  /  1
         MOVE  HKK-F12       TO   KEI-TORAY-2-1
         COMPUTE  KEI-TORAY-2-2 = KEI-TORAY-2-1  /  2
         MOVE  HKK-F13       TO   KEI-TORAY-3-1
         COMPUTE  KEI-TORAY-3-2 = KEI-TORAY-3-1  /  3
         MOVE  HKK-F14       TO   KEI-TORAY-4-1
         COMPUTE  KEI-TORAY-4-2 = KEI-TORAY-4-1  /  4
         COMPUTE  KEI-TORAY-5-2 = KEI-TORAY-1-2 +
                                  KEI-TORAY-2-2 +
                                  KEI-TORAY-3-2 +
                                  KEI-TORAY-4-2
         COMPUTE  KEI-TORAY-5-2 = KEI-TORAY-5-2 + 0.9
         MOVE     KEI-TORAY-5-2 TO WK-KONPO  HKK-F09
**        DISPLAY "HKK-F081= " HKK-F08 UPON CONS
         WRITE  HKK-REC
     ELSE
         MOVE  JOH-F02      TO  HKK-F02
         MOVE  JOH-F03      TO  HKK-F03
         MOVE  JOH-F04      TO  HKK-F04
*##      ADD   WK-TORAY     TO  HAK-F11
*総梱数算出
         IF    NFSHOMS1-INV-FLG = "   "
               MOVE  SHO-F10 TO WK-SHO-F10
               EVALUATE  WK-SHO-F10
                   WHEN  1   ADD   WK-TORAY TO HKK-F11
                   WHEN  2   ADD   WK-TORAY TO HKK-F12
                   WHEN  3   ADD   WK-TORAY TO HKK-F13
                   WHEN  4   ADD   WK-TORAY TO HKK-F14
                   WHEN OTHER ADD  WK-TORAY TO HKK-F11
                        DISPLAY NC"＃箱数算出値取得エラー＃"
                        UPON CONS
               END-EVALUATE
*##      ELSE
*##            ADD   WK-TORAY               TO HKK-F11
         END-IF
*********総梱包数計算
         MOVE  HKK-F11       TO   KEI-TORAY-1-1
         COMPUTE  KEI-TORAY-1-2 = KEI-TORAY-1-1  /  1
         MOVE  HKK-F12       TO   KEI-TORAY-2-1
         COMPUTE  KEI-TORAY-2-2 = KEI-TORAY-2-1  /  2
         MOVE  HKK-F13       TO   KEI-TORAY-3-1
         COMPUTE  KEI-TORAY-3-2 = KEI-TORAY-3-1  /  3
         MOVE  HKK-F14       TO   KEI-TORAY-4-1
         COMPUTE  KEI-TORAY-4-2 = KEI-TORAY-4-1  /  4
         COMPUTE  KEI-TORAY-5-2 = KEI-TORAY-1-2 +
                                  KEI-TORAY-2-2 +
                                  KEI-TORAY-3-2 +
                                  KEI-TORAY-4-2
         COMPUTE  KEI-TORAY-5-2 = KEI-TORAY-5-2 + 0.9
         MOVE     KEI-TORAY-5-2 TO WK-KONPO  HKK-F09
**        DISPLAY "HKK-F082= " HKK-F08 UPON CONS
         REWRITE  HKK-REC
     END-IF.
*
 NEWKONPOU-KEISAN-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
