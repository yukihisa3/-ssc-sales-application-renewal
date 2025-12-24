# SSY9304B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY9304B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　ＡＭＡＺＯＮ　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　ＡＭＡＺＯＮ　出荷確認データ抽出  *
*    作成日／作成者　　　：　2020/11/13 INOUE                  *
*    処理概要　　　　　　：　ＥＯＳ名人と受け渡しを行うＡＭＡ　*
*                            ＺＯＮ向け受注・出荷予定・検品実　*
*                            績を、ＥＸＣＥＬで確認するための　*
*                            前処理。　　　　　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9304B.
*                  流用:SSY9302B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/11/12.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< ＡＭＡＺＯＮ基本情報ファイル >>----*
     SELECT   AMZJOHL2  ASSIGN              DA-01-VI-AMZJOHL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       JOH-FE01  JOH-FE02
                                            JOH-FE03  JOH-FB09
                                            JOH-FA04
                        FILE      STATUS    JOH-STATUS.
*----<< ＡＭＡＺＯＮ出荷確認データ >>----*
     SELECT   AMZCHKWK   ASSIGN    TO       DA-01-VS-AMZCHKWK
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE      STATUS    CHK-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    ＡＭＡＺＯＮ基本情報ファイル
******************************************************************
 FD  AMZJOHL2            LABEL RECORD   IS   STANDARD.
     COPY     AMZJOHL2   OF       XFDLIB
              JOINING   JOH       PREFIX.
*
******************************************************************
*    ＡＭＡＺＯＮ発注出荷確認データ
******************************************************************
 FD  AMZCHKWK            LABEL RECORD   IS   STANDARD.
     COPY     AMZCHKWK   OF        XFDLIB
              JOINING    CHK       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP1-CNT           PIC  9(08)     VALUE  ZERO.
     03  SKIP2-CNT           PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  AMZCHKWK-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  SHTDENL1-INV-FLG    PIC  X(03)     VALUE  SPACE.
 01  WK-GYO-CNT              PIC  9(02)     VALUE  ZERO.
 01  WK-KMS-F06              PIC  9(09)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME           PIC   9(08)  VALUE  ZERO.
*
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
     03  CHK-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY9304B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9304B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9304B".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-IN-JDATE             PIC   9(08).
 01  PARA-IN-JTIME             PIC   9(04).
 01  PARA-IN-TORICD            PIC   9(08).
 01  PARA-IN-SOKO              PIC   X(02).
 01  PARA-IN-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JDATE
                                       PARA-IN-JTIME
                                       PARA-IN-TORICD
                                       PARA-IN-SOKO
                                       PARA-IN-NOUDT.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZJOHL2.
     MOVE      "AMZJOHL2 "   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   AMZCHKWK.
     MOVE      "AMZCHKWK"   TO   AB-FILE.
     MOVE      CHK-STATUS   TO   AB-STS.
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
              UNTIL     END-FLG   =  "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     AMZJOHL2.
     OPEN     OUTPUT    AMZCHKWK.
*
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
     MOVE     SPACE     TO        WK-INV-FLG.
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
*   システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*
*    ＡＭＡＺＯＮ基本情報ファイルスタート
     MOVE     SPACE          TO   JOH-REC.
     INITIALIZE                   JOH-REC.
     MOVE     PARA-IN-JDATE  TO   JOH-FE01.
     MOVE     PARA-IN-JTIME  TO   JOH-FE02.
     MOVE     PARA-IN-TORICD TO   JOH-FE03.
     MOVE     PARA-IN-SOKO   TO   JOH-FB09.
     MOVE     PARA-IN-NOUDT  TO   JOH-FA04.
     START    AMZJOHL2  KEY  >=   JOH-FE01   JOH-FE02
                                  JOH-FE03   JOH-FB09
                                  JOH-FA04
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    ＡＭＡＺＯＮ基本情報ファイル読込み
     PERFORM AMZJOHL2-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 AMZJOHL2-READ-SEC    SECTION.
*
     MOVE    "AMZJOHL2-READ-SEC"    TO  S-NAME.
*
     READ     AMZJOHL2
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  AMZJOHL2-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*    バッチ番号のチェック
     IF       PARA-IN-JDATE  =  JOH-FE01
     AND      PARA-IN-JTIME  =  JOH-FE02
     AND      PARA-IN-TORICD =  JOH-FE03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  AMZJOHL2-READ-EXIT
     END-IF.
*    倉庫ＣＤチェック
     IF       PARA-IN-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-IN-SOKO  =  JOH-FB09
                   CONTINUE
              ELSE
                   GO            TO  AMZJOHL2-READ-SEC
              END-IF
     END-IF.
*    納品日のチェック
     IF       PARA-IN-NOUDT  =  ZERO
              CONTINUE
     ELSE
              IF  PARA-IN-NOUDT  =  JOH-FA04
                  CONTINUE
              ELSE
                  GO        TO   AMZJOHL2-READ-SEC
              END-IF
     END-IF.
*
 AMZJOHL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
 MAIN010.
*    データ作成
     PERFORM  AMZCHKWK-WRITE-SEC.
*
 MAIN020.
*    ＡＭＡＺＯＮ基本情報ファイル読込み
     PERFORM AMZJOHL2-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ＡＭＡＺＯＮ出荷確認データ抽出処理
****************************************************************
 AMZCHKWK-WRITE-SEC     SECTION.
*
     MOVE     "AMZCHKWK-WRITE-SEC"  TO   S-NAME.
*
     MOVE      SPACE                TO   CHK-REC.
     INITIALIZE                          CHK-REC.
*
*  共通情報フィールド
     MOVE      JOH-F0               TO   CHK-F0.
*
*  受注情報フィールド
     MOVE      JOH-FA               TO   CHK-FA.
*
*  引当情報フィールド
     MOVE      JOH-FB               TO   CHK-FB.
*
*  検品実績フィールド
     MOVE      JOH-FC               TO   CHK-FC.
*
*  予備フィールド
     MOVE      JOH-FD               TO   CHK-FD.
*
*  付加情報フィールド
     MOVE      JOH-FE               TO   CHK-FE.
*
     WRITE     CHK-REC.
     ADD       1                    TO   WRT-CNT.
*
 AMZCHKWK-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "AMAZONｷﾎﾝｼﾞｮｳﾎｳ READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "AMAZONｶｸﾆﾝﾜｰｸ   WRT  CNT = " WRT-CNT   UPON CONS.
*
     CLOSE     AMZJOHL2  AMZCHKWK.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
