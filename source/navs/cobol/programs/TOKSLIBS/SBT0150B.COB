****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷連携サブシステム　　　　　　　*
*    業務名　　　　　　　：　業務改善                          *
*    モジュール名　　　　：　オンライン連携状況ファイル作成    *
*    作成日／更新日　　　：　2012/10/09                        *
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　*
*    処理概要　　　　　　：　受け取った各パラメタより、売上伝  *
*                            番ファイルを読み、倉庫毎に伝票件  *
*                            数／伝票枚数をカウントする。      *
*　　更新日／更新者　　　：                                    *
*                        ：                                    *
*    変更概要　　　　　　：　　　　　　　　　　　　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBT0150B.
 AUTHOR.                T.MIURA.
 DATE-WRITTEN.          12/10/09.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票データ
     SELECT   SHTDENF   ASSIGN    TO        DA-01-VI-SHTDENLJ
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46   DEN-F47
                                            DEN-F01   DEN-F112
                                            DEN-F48   DEN-F02
                                            DEN-F04   DEN-F051
                                            DEN-F07   DEN-F03
                        FILE  STATUS   IS   DEN-STATUS.
*オンライン連携状況ファイル
     SELECT   LNKONLF   ASSIGN    TO        DA-01-VI-LNKONLL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   LNK-F01
                                                 LNK-F02
                                                 LNK-F03
                                                 LNK-F04
                                                 LNK-F05
                        FILE      STATUS    IS   LNK-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TOK-F01
                        FILE      STATUS    IS   TOK-STATUS.
*倉庫マスタ
     SELECT   ZSOKMS1   ASSIGN    TO        DA-01-VI-ZSOKMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       SOK-F01
                        FILE      STATUS    IS   SOK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  SHTDENF
                        LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN  AS   PREFIX.
*
******************************************************************
*    オンライン連携状況ファイル
******************************************************************
 FD  LNKONLF            LABEL RECORD   IS   STANDARD.
     COPY     LNKONLF   OF        XFDLIB
              JOINING   LNK       PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
******************************************************************
*    倉庫マスタ
******************************************************************
 FD  ZSOKMS1            LABEL RECORD   IS   STANDARD.
     COPY     ZSOKMS    OF        XFDLIB
              JOINING   SOK       PREFIX.
*****************************************************************
 WORKING-STORAGE        SECTION.
*   <ｶｳﾝﾄ>
 01  END-FLG                 PIC   X(03)     VALUE  SPACE.
*   <INV-FLG>
 01  TOKMS2-INV-FLG          PIC   X(03)     VALUE  SPACE.
 01  ZSOKMS1-INV-FLG         PIC   X(03)     VALUE  SPACE.
*   <ﾌﾞﾚｰｸｷｰ>
 01  WK-DEN-F112             PIC   9(08)     VALUE  ZERO.
 01  HEN-DEN-F112            PIC   9(08)     VALUE  ZERO.
 01  WK-DEN-F48              PIC   X(02)     VALUE  SPACE.
 01  WK-DEN-F02              PIC   9(09)     VALUE  ZERO.
*   <件数ｶｳﾝﾀｰ>
 01  CNT-DENMEI              PIC   9(07)     VALUE  ZERO.
 01  CNT-DENPYO              PIC   9(07)     VALUE  ZERO.
 01  CNT-RNK                 PIC   9(07)     VALUE  ZERO.
 01  CNT-MIRNK               PIC   9(07)     VALUE  ZERO.
*   <ｴﾗｰNO.>
*01  PARA-ERRNO              PIC   X(01)     VALUE  SPACE.
*   <ｽﾃｰﾀｽ>
 01  WK-ST.
     03  DEN-STATUS          PIC   X(02).
     03  LNK-STATUS          PIC   X(02).
     03  TOK-STATUS          PIC   X(02).
     03  SOK-STATUS          PIC   X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  ST-PG           PIC   X(08)  VALUE "SBT0150B".
         05  FILLER          PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SBT0150B".
         05  FILLER          PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  END-PG          PIC   X(08)  VALUE "SBT0150B".
         05  FILLER          PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  AB-FILE         PIC   X(08).
         05  FILLER          PIC   X(06)  VALUE " ST = ".
         05  AB-STS          PIC   X(02).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(07)  VALUE " SEC = ".
         05  S-NAME          PIC   X(30).
     03  MSG-IN.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT          PIC   9(07).
         05  FILLER          PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER          PIC   X(05)  VALUE " *** ".
         05  FILLER          PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT         PIC   9(07).
         05  FILLER          PIC   X(05)  VALUE " *** ".
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMN       PIC  9(04).
     03  SYS-SSMS       PIC  9(04).
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE              PIC   9(08).
 01  PARA-JTIME              PIC   9(04).
 01  PARA-TOKCD              PIC   9(08).
 01  PARA-ERRNO              PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TOKCD
                                       PARA-ERRNO.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENF.
     MOVE      "SHTDENLJ"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   LNKONLF.
     MOVE      "LNKONLL1"   TO   AB-FILE.
     MOVE      LNK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     DISPLAY   "LNK-F01 = " LNK-F01 UPON CONS.
     DISPLAY   "LNK-F02 = " LNK-F02 UPON CONS.
     DISPLAY   "LNK-F03 = " LNK-F03 UPON CONS.
     DISPLAY   "LNK-F04 = " LNK-F04 UPON CONS.
     DISPLAY   "LNK-F05 = " LNK-F05 UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSOKMS1.
     MOVE      "ZSOKMS1"    TO   AB-FILE.
     MOVE      SOK-STATUS   TO   AB-STS.
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
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     SHTDENF  TOKMS2  ZSOKMS1.
     OPEN     I-O       LNKONLF.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*ワーク初期化
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     ZERO      TO        CNT-DENMEI CNT-DENPYO.
     MOVE     ZERO      TO        CNT-RNK    CNT-MIRNK.
*取引先マスタ索引
     PERFORM TOKMS2-READ-SEC.
*
     IF  TOKMS2-INV-FLG = "INV"
         DISPLAY NC"＃＃連携対象無　取引先１＃＃" UPON CONS
         MOVE "1"       TO        PARA-ERRNO
         STOP RUN
*    ELSE
*        IF  TOK-FIL1(3:1) = SPACE
*            DISPLAY NC"＃＃連携対象無　取引先２＃＃" UPON CONS
*            MOVE "2"       TO        PARA-ERRNO
*            STOP RUN
*        END-IF
     END-IF.
*売上伝票ファイルスタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-JDATE     TO   DEN-F46.
     MOVE     PARA-JTIME     TO   DEN-F47.
     MOVE     PARA-TOKCD     TO   DEN-F01.
     START    SHTDENF   KEY  >=   DEN-F46   DEN-F47
                                  DEN-F01   DEN-F112
                                  DEN-F48   DEN-F02
                                  DEN-F04   DEN-F051
                                  DEN-F07   DEN-F03
         INVALID   KEY
         MOVE "END"     TO   END-FLG
         DISPLAY NC"＃＃連携対象無　売上伝票＃＃" UPON CONS
         MOVE "3"       TO        PARA-ERRNO
         STOP RUN
     END-START.
*
     PERFORM  SHTDENF-READ-SEC.
*
     IF       END-FLG = "END"
              DISPLAY NC"＃＃連携対象無　売上伝票＃＃" UPON CONS
              MOVE "4"       TO        PARA-ERRNO
              STOP RUN
     ELSE
              MOVE DEN-F112  TO        WK-DEN-F112
              MOVE DEN-F48   TO        WK-DEN-F48
              MOVE ZERO      TO        CNT-DENMEI
              MOVE ZERO      TO        CNT-DENPYO
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    納品日／倉庫のブレイクチェック
     MOVE     DEN-F112            TO   HEN-DEN-F112.
     IF   HEN-DEN-F112  =  WK-DEN-F112
     AND  DEN-F48       =  WK-DEN-F48
          CONTINUE
     ELSE
          PERFORM  LNKONLF-WRITE-SEC
          MOVE DEN-F112           TO   WK-DEN-F112
          MOVE DEN-F48            TO   WK-DEN-F48
          MOVE ZERO               TO   CNT-DENMEI
          MOVE ZERO               TO   CNT-DENPYO
          MOVE ZERO               TO   CNT-RNK
          MOVE ZERO               TO   CNT-MIRNK
     END-IF.
*    伝票明細カウント
     ADD       1                  TO   CNT-DENMEI.
*    連携件数／未連携件数カウント
     IF   DEN-F68           =  "1"
          ADD  1                  TO   CNT-RNK
     ELSE
          ADD  1                  TO   CNT-MIRNK
     END-IF.
*    伝票番号ブレイク
     IF   DEN-F02       NOT =  WK-DEN-F02
          ADD  1                  TO   CNT-DENPYO
          MOVE DEN-F02            TO   WK-DEN-F02
     END-IF.
*売上伝票ファイル読込
     PERFORM  SHTDENF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読み込み
****************************************************************
 SHTDENF-READ-SEC     SECTION.
*
     READ     SHTDENF
              AT END       MOVE "END"     TO   END-FLG
                           GO             TO   SHTDENF-READ-EXIT
              NOT AT END   ADD   1        TO   IN-CNT
     END-READ.
*    読込経過表示
     IF       IN-CNT(4:4) = "0000" OR "5000"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
              DISPLAY "倉庫  = "  WK-DEN-F48  UPON CONS
              DISPLAY "納品日= "  WK-DEN-F112 UPON CONS
     END-IF.
*    バッチ番号チェック
     IF       PARA-JDATE  NOT =  DEN-F46
     OR       PARA-JTIME  NOT =  DEN-F47
     OR       PARA-TOKCD  NOT =  DEN-F01
              MOVE "END"         TO    END-FLG
     END-IF.
*
 SHTDENF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　オンライン連携状況ファイル作成
****************************************************************
 LNKONLF-WRITE-SEC    SECTION.
*    ＜倉庫ＣＤ対象チェック＞
     PERFORM ZSOKMS1-READ-SEC.
     IF  ZSOKMS1-INV-FLG = "INV"
         GO              TO       LNKONLF-WRITE-EXIT
     END-IF.
*
     MOVE      SPACE     TO       LNK-REC.
     INITIALIZE                   LNK-REC.
*バッチ番号セット
     MOVE  PARA-JDATE    TO       LNK-F01.
     MOVE  PARA-JTIME    TO       LNK-F02.
     MOVE  PARA-TOKCD    TO       LNK-F03.
*倉庫ＣＤ
     MOVE  WK-DEN-F48    TO       LNK-F04.
*納品日
     MOVE  WK-DEN-F112   TO       LNK-F05.
*データ件数
     MOVE  CNT-DENMEI    TO       LNK-F06.
*伝票枚数
     MOVE  CNT-DENPYO    TO       LNK-F07.
*連携件数
     MOVE  CNT-RNK       TO       LNK-F08.
*未連携件数
     MOVE  CNT-MIRNK     TO       LNK-F09.
*作成日付
     MOVE  SYS-DATEW     TO       LNK-F96.
*作成時刻
     MOVE  SYS-TIME(1:6) TO       LNK-F97.
*オンライン状況連携ファイル出力
     WRITE LNK-REC
*出力件数カウント
     ADD   1             TO       OUT-CNT.
*
 LNKONLF-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　取引先マスタ読み込み
****************************************************************
 TOKMS2-READ-SEC       SECTION.
*
     MOVE     SPACE          TO   TOK-REC
     INITIALIZE                   TOK-REC
     MOVE     PARA-TOKCD     TO   TOK-F01
     READ     TOKMS2
              INVALID
              MOVE "INV"     TO   TOKMS2-INV-FLG
              NOT  INVALID
              MOVE SPACE     TO   TOKMS2-INV-FLG
     END-READ.
*
 TOKMS2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　倉庫マスタ読み込み
****************************************************************
 ZSOKMS1-READ-SEC      SECTION.
*
     MOVE     SPACE          TO   SOK-REC
     INITIALIZE                   SOK-REC
     MOVE     WK-DEN-F48     TO   SOK-F01
     READ     ZSOKMS1
              INVALID
              MOVE "INV"     TO   ZSOKMS1-INV-FLG
              GO             TO   ZSOKMS1-READ-EXIT
              NOT  INVALID
              MOVE SPACE     TO   ZSOKMS1-INV-FLG
     END-READ.
*    倉庫ＣＤが物流連携対象かチェック
     IF    SOK-F13  = SPACE
              MOVE "INV"     TO   ZSOKMS1-INV-FLG
     END-IF.
*
 ZSOKMS1-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*明細件数が０以上の場合、出力
     IF   CNT-DENMEI > ZERO
          PERFORM LNKONLF-WRITE-SEC
     END-IF.
*作成件数表示
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     SHTDENF  LNKONLF TOKMS2 ZSOKMS1.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
