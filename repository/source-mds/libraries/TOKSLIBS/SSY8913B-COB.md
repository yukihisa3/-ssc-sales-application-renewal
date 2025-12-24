# SSY8913B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY8913B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　リック　　　　　　ＷＥＢＥＤＩ　　*
*    業務名　　　　　　　：　リック　　　　　　ＷＥＢＥＤＩ　　*
*    モジュール名　　　　：　納品予定データ送信形式変換        *
*    作成日／更新日　　　：　2008/04/14                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY8913B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          08/04/14.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*リック　納品予定データ
     SELECT   RCSYUKF   ASSIGN    TO        DA-01-VI-RCSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RCS-K01   RCS-K02
                                            RCS-K03   RCS-K04
                                            RCS-K08   RCS-K06
                                            RCS-K07
                        FILE      STATUS    RCS-STATUS.
*リック　発注情報データ
     SELECT   RCJOHOF   ASSIGN    TO        DA-01-VI-RCJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RCJ-K01   RCJ-K02
                                            RCJ-K03   RCJ-K04
                                            RCJ-K05   RCJ-K06
                                            RCJ-K07   RCJ-K08
                        FILE      STATUS    RCJ-STATUS.
*リック　送信ファイル
     SELECT   SNDRIC  ASSIGN    TO        DA-01-S-SNDRIC
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    リック　納品予定ファイル
******************************************************************
 FD  RCSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     RCSYUKF   OF        XFDLIB
              JOINING   RCS       PREFIX.
******************************************************************
*    リック　発注情報データ
******************************************************************
 FD  RCJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     RCJOHOF   OF        XFDLIB
              JOINING   RCJ       PREFIX.
******************************************************************
*    リック　送信ファイル
******************************************************************
 FD  SNDRIC
                        BLOCK CONTAINS 1 RECORDS.
 01  SND-REC            PIC  X(3141).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RCJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT             PIC  9(08)     VALUE  ZERO.
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-TAIL-02              PIC  9(11)     VALUE  ZERO.
 01  WK-TAIL-03              PIC  9(11)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  SND-STATUS        PIC  X(02).
     03  RCS-STATUS        PIC  X(02).
     03  RCJ-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY8913B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8913B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY8913B".
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
*テイル情報
 01  WK-TAIL-REC.
     03  TAIL-01            PIC   X(02).
     03  TAIL-02            PIC   9(11).
     03  FILLER             PIC   X(11).
     03  TAIL-03            PIC   9(11).
     03  FILLER             PIC   X(09).
     03  TAIL-04            PIC   X(01).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT.
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCSYUKF.
     MOVE      "RCSYUKL2"    TO   AB-FILE.
     MOVE      RCS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDRIC.
     MOVE      "SNDRIC"    TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   RCJOHOF.
     MOVE      "RCJOHOL1"    TO   AB-FILE.
     MOVE      RCJ-STATUS   TO   AB-STS.
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
     OPEN     I-O       RCSYUKF   RCJOHOF.
     OPEN     EXTEND    SNDRIC.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FLG   WK-CNT.
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
*    リック　　　　　出荷情報スタート
     MOVE     SPACE          TO   RCS-REC.
     INITIALIZE                   RCS-REC.
     MOVE     PARA-JDATE     TO   RCS-K01.
     MOVE     PARA-JTIME     TO   RCS-K02.
     MOVE     PARA-TORICD    TO   RCS-K03.
     MOVE     PARA-SOKO      TO   RCS-K04.
     MOVE     PARA-NOUDT     TO   RCS-K08.
     START    RCSYUKF   KEY  >=   RCS-K01   RCS-K02
                                  RCS-K03   RCS-K04
                                  RCS-K08
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    リック　　　　　出荷情報保存データ読込み
     PERFORM RCSYUKF-READ-SEC.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷情報データ　ＲＥＡＤ　　　　　　　　　　　　*
****************************************************************
 RCSYUKF-READ-SEC    SECTION.
*
     READ     RCSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  RCSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
     IF    READ-CNT(6:3)  =  "000" OR "500"
           DISPLAY "READ-CNT = " READ-CNT  UPON CONS
     END-IF.
*    送信ＦＬＧのチェック
     IF       RCS-K11  =  "1"
              GO                 TO   RCSYUKF-READ-SEC
     END-IF.
*    バッチ番号のチェック
     IF       PARA-JDATE  =  RCS-K01
     AND      PARA-JTIME  =  RCS-K02
     AND      PARA-TORICD =  RCS-K03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  RCSYUKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  RCS-K04
                       CONTINUE
              ELSE
                       GO        TO   RCSYUKF-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  RCS-K08
                       CONTINUE
              ELSE
                       GO        TO   RCSYUKF-READ-SEC
              END-IF
     END-IF.
*
 RCSYUKF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*ヘッダ部出力
     MOVE     "0"                 TO   RCS-A20(347:1).
*****MOVE     RCS-K20             TO   SND-REC.
     IF       SND-REC(23:1)   =   ZERO
     AND      SND-REC(22:1)   =   SPACE
              MOVE     SPACE      TO   SND-REC(23:1)
     END-IF.
*    ヘッダを書き込むかどうか判断します。
     IF       RCS-K06    =   WK-DENNO
              CONTINUE
     ELSE
**************テイル行出力するか判断
              IF  SND-CNT   >  ZERO
                  MOVE SPACE      TO   SND-REC
                  INITIALIZE           SND-REC
                  MOVE SPACE      TO   WK-TAIL-REC
                  INITIALIZE           WK-TAIL-REC
                  MOVE "TR"       TO   TAIL-01
                  MOVE WK-TAIL-02 TO   TAIL-02
                  MOVE WK-TAIL-03 TO   TAIL-03
                  MOVE "0"        TO   TAIL-04
                  MOVE WK-TAIL-REC TO  SND-REC
                  WRITE SND-REC
                  ADD  1          TO   SND-CNT
                  MOVE ZERO       TO   WK-TAIL-02 WK-TAIL-03
              END-IF
              MOVE     RCS-K20    TO   SND-REC
              WRITE    SND-REC
              ADD      1          TO   SND-CNT
              MOVE     RCS-K06    TO   WK-DENNO
     END-IF.
*明細部出力
*    送信ファイル出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*    ダミーデータセット
     MOVE     "0"                 TO   RCS-A36(70:1).
*****COMPUTE  RCS-M16  =  RCS-M16 * 100.
     MOVE     RCS-K21             TO   SND-REC.
     ADD      RCS-M14             TO   WK-TAIL-02.
     ADD      RCS-M15             TO   WK-TAIL-03.
     WRITE    SND-REC.
     ADD      1                   TO   SND-CNT.
*情報データにも書込み
     MOVE     SPACE               TO   RCJ-REC.
     INITIALIZE                        RCJ-REC.
     MOVE     RCS-K01             TO   RCJ-K01.
     MOVE     RCS-K02             TO   RCJ-K02.
     MOVE     RCS-K03             TO   RCJ-K03.
     MOVE     RCS-K04             TO   RCJ-K04.
     MOVE     RCS-K05             TO   RCJ-K05.
     MOVE     RCS-K06             TO   RCJ-K06.
     MOVE     RCS-K07             TO   RCJ-K07.
     MOVE     RCS-K08             TO   RCJ-K08.
     READ     RCJOHOF  INVALID
              MOVE     "INV"      TO   RCJOHOF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   RCJOHOF-INV-FLG
     END-READ.
*
     IF       RCJOHOF-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   RCJ-K12
              MOVE     "1"                 TO   RCJ-K11
              REWRITE  RCJ-REC
     END-IF.
*送信情報セット
     MOVE     SYS-DATEW           TO   RCS-K12.
     MOVE     "1"                 TO   RCS-K11.
     REWRITE  RCS-REC.
*
 MAIN010.
*    リック　出荷情報保存データ読込み
     PERFORM RCSYUKF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
**************テイル行出力するか判断
     IF  SND-CNT   >  ZERO
         MOVE SPACE      TO   SND-REC
         INITIALIZE           SND-REC
         MOVE SPACE      TO   WK-TAIL-REC
         INITIALIZE           WK-TAIL-REC
         MOVE "TR"       TO   TAIL-01
         MOVE WK-TAIL-02 TO   TAIL-02
         MOVE WK-TAIL-03 TO   TAIL-03
         MOVE "0"        TO   TAIL-04
         MOVE WK-TAIL-REC TO  SND-REC
         WRITE SND-REC
         ADD  1          TO   SND-CNT
     END-IF.
*
     DISPLAY "ﾘｯｸｼｭｯｶ   READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ﾘｯｸｼｭｯｶ ｿｳｼﾝ   CNT = " SND-CNT    UPON CONS.
*
     CLOSE    RCSYUKF  RCJOHOF  SNDRIC.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
