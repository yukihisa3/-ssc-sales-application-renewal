****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　出荷　　　　　　　　　　　　　　  *
*    サブシステム　　　　：　ヨドバシ　　　ＥＤＩ　　　　　　  *
*    モジュール名　　　　：　ヨドバシ　　　出荷通知データ抽出  *
*    作成日／作成者　　　：　2023/02/01 INOUE                  *
*    処理概要　　　　　　：　条件に従い出荷通知作成用データを  *
*                            抽出する。出荷通知データ作成の　　*
*                            前処理。　　　　　　　　　　　　　*
*    更新履歴                                                  *
*    更新日／更新者　　　：　                                  *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY9521B.
*                  流用:SSY9472B←これは廃盤
 AUTHOR.                NAV.
 DATE-WRITTEN.          2023/02/01.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 売上伝票ファイル >>----*
     SELECT   SHTDENLJ  ASSIGN              DA-01-VI-SHTDENLJ
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DEN-F46
                                            DEN-F47
                                            DEN-F01
                                            DEN-F112
                                            DEN-F48
                                            DEN-F02
                                            DEN-F04
                                            DEN-F051
                                            DEN-F07
                                            DEN-F03
                        FILE      STATUS    DEN-STATUS.
*----<< ヨドバシ　　基本情報ファイル >>----*
     SELECT   YODJOHL2  ASSIGN              DA-01-VI-YODJOHL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JOH-F15
                                            JOH-F16
                                            JOH-F17
                                            JOH-F03
                                            JOH-F18
                                            JOH-F19
                        FILE      STATUS    JOH-STATUS.
*----<< ヨドバシ　　出荷通知抽出データ >>----*
     SELECT   YODSNDL3   ASSIGN    TO       DA-01-VI-YODSNDL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       SND-F11
                                            SND-F12
                                            SND-F13
                                            SND-F16
                                            SND-F02
                        FILE      STATUS    SND-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    売上伝票ファイル
******************************************************************
 FD  SHTDENLJ            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENLJ   OF        XFDLIB
              JOINING    DEN       PREFIX.
******************************************************************
*    ヨドバシ　　基本情報ファイル
******************************************************************
 FD  YODJOHL2            LABEL RECORD   IS   STANDARD.
     COPY     YODJOHL2   OF       XFDLIB
              JOINING   JOH       PREFIX.
*
******************************************************************
*    ヨドバシ　　発注出荷通知抽出データ
******************************************************************
 FD  YODSNDL3            LABEL RECORD   IS   STANDARD.
     COPY     YODSNDL3   OF        XFDLIB
              JOINING    SND       PREFIX.
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
     03  WRT-CNT             PIC  9(08)     VALUE  ZERO.
     03  KMK2-CNT            PIC  9(08)     VALUE  ZERO.
     03  KMS-CNT             PIC  9(08)     VALUE  ZERO.
     03  KAKUTE-CNT          PIC  9(08)     VALUE  ZERO.
     03  TAISYO-CNT          PIC  9(08)     VALUE  ZERO.
 01  WK-INV-FLG.
     03  YODJOHL2-INV-FLG    PIC  X(03)     VALUE  SPACE.
     03  YODSNDL3-INV-FLG    PIC  X(03)     VALUE  SPACE.
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
     03  SND-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY9521B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9521B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY9521B".
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
 01  PARA-IN-SNOUDT            PIC   9(08).
 01  PARA-IN-ENOUDT            PIC   9(08).
 01  PARA-OUT-CNT              PIC   9(07).
 01  PARA-OUT-KAKTEI           PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-IN-JDATE
                                       PARA-IN-JTIME
                                       PARA-IN-TORICD
                                       PARA-IN-SOKO
                                       PARA-IN-SNOUDT
                                       PARA-IN-ENOUDT
                                       PARA-OUT-CNT
                                       PARA-OUT-KAKTEI.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHTDENLJ.
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
                        PROCEDURE   YODJOHL2.
     MOVE      "YODJOHL2"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   YODSNDL3.
     MOVE      "YODSNDL3"   TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
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
     OPEN     INPUT     SHTDENLJ.
     OPEN     I-O       YODJOHL2.
     OPEN     I-O       YODSNDL3.
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
 INIT-01.
*    売上伝票ファイルスタート
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
     MOVE     PARA-IN-JDATE  TO   DEN-F46.
     MOVE     PARA-IN-JTIME  TO   DEN-F47.
     MOVE     PARA-IN-TORICD TO   DEN-F01.
     MOVE     PARA-IN-SNOUDT TO   DEN-F112.
     MOVE     PARA-IN-SOKO   TO   DEN-F48.
     START    SHTDENLJ  KEY  >=   DEN-F46    DEN-F47   DEN-F01
                                  DEN-F112   DEN-F48   DEN-F02
                                  DEN-F04    DEN-F051  DEN-F07
                                  DEN-F03
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO             TO   INIT-EXIT
     END-START.
*
*    売上伝票ファイル読込み
     PERFORM SHTDENLJ-READ-SEC.
     IF      END-FLG  =    "END"
             GO             TO   INIT-EXIT
     END-IF.
*
 INIT-02.
*    出荷通知抽出ファイルスタート
     MOVE     SPACE          TO   SND-REC.
     INITIALIZE                   SND-REC.
     MOVE     PARA-IN-JDATE  TO   SND-F11.
     MOVE     PARA-IN-JTIME  TO   SND-F12.
     MOVE     PARA-IN-TORICD TO   SND-F13.
     MOVE     PARA-IN-SNOUDT TO   SND-F02.
     MOVE     PARA-IN-SOKO   TO   SND-F16.
     START    YODSNDL3  KEY  >=   SND-F11    SND-F12   SND-F13
                                  SND-F16    SND-F02
         INVALID   KEY
              GO             TO   INIT-04
         NOT INVALID
              GO             TO   INIT-03
     END-START.
*
 INIT-03.
*    出荷通知抽出ファイル読込み
     READ    YODSNDL3 NEXT
        AT END  GO             TO   INIT-04
     END-READ.
*    バッチＮＯブレイク
     IF    ( SND-F11 NOT =  PARA-IN-JDATE  ) OR
           ( SND-F12 NOT =  PARA-IN-JTIME  ) OR
           ( SND-F13 NOT =  PARA-IN-TORICD )
             GO             TO   INIT-04
     END-IF.
*    倉庫ＣＤブレイク
     IF      SND-F16 NOT =  SPACE
         IF  SND-F16 NOT =  PARA-IN-SOKO
             GO             TO   INIT-04
         END-IF
     END-IF.
*    納品日範囲チェック
     IF    ( SND-F02     <  PARA-IN-SNOUDT ) OR
           ( SND-F02     >  PARA-IN-ENOUDT )
             GO             TO   INIT-04
     END-IF.
*    倉庫未確定分は削除
     IF      SND-F18 NOT =  ZERO
             ADD  1      TO KAKUTE-CNT
*T
*            DISPLAY "KAKUTE-CNT=" KAKUTE-CNT UPON CONS
*T
             GO             TO   INIT-03
     ELSE
             ADD  1      TO TAISYO-CNT
*T
*            DISPLAY "TAISYO-CNT=" TAISYO-CNT UPON CONS
*T
             DELETE         YODSNDL3
             GO             TO   INIT-03
     END-IF.
*    全件倉庫確定済みの場合は再発行扱い
 INIT-04.
*T
*    DISPLAY "KAKUTE-CNT=" KAKUTE-CNT UPON CONS.
*    DISPLAY "TAISYO-CNT=" TAISYO-CNT UPON CONS.
*T
     IF    ( KAKUTE-CNT NOT = ZERO ) AND
           ( TAISYO-CNT     = ZERO )
             MOVE     "1"           TO  PARA-OUT-KAKTEI
             MOVE     "END"         TO  END-FLG
             GO                     TO  INIT-EXIT
     ELSE
             MOVE     "0"           TO  PARA-OUT-KAKTEI
     END-IF.
*
     CLOSE    YODSNDL3.
     OPEN I-O YODSNDL3.
*
*    MOVE     DEN-F01               TO  JOH-F17.
*    MOVE     DEN-F111              TO  JOH-F03.
*    MOVE     DEN-F02               TO  JOH-F18.
*    MOVE     DEN-F03               TO  JOH-F19.
*    PERFORM  YODJOHL2-READ-SEC.
*    IF       YODJOHL2-INV-FLG      =   "INV"
*             MOVE    "END"         TO   END-FLG
*    END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　売上伝票ファイル読込み　　　　　　　　　　　　　*
****************************************************************
 SHTDENLJ-READ-SEC    SECTION.
*
     MOVE    "SHTDENLJ-READ-SEC"    TO  S-NAME.
*
 SHTDENLJ-READ-01.
     READ     SHTDENLJ
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  SHTDENLJ-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
 SHTDENLJ-READ-02.
*    バッチ番号のチェック
     IF       PARA-IN-JDATE  =  DEN-F46
     AND      PARA-IN-JTIME  =  DEN-F47
     AND      PARA-IN-TORICD =  DEN-F01
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  SHTDENLJ-READ-EXIT
     END-IF.
*
 SHTDENLJ-READ-03.
*    納品日のチェック
*****IF       PARA-IN-SNOUDT  =  ZERO
*             CONTINUE
*    ELSE
*             IF  PARA-IN-SNOUDT  =  DEN-F112
*                 CONTINUE
*             ELSE
*                 GO            TO  SHTDENLJ-READ-SEC
*             END-IF
*****END-IF.
*****納品日チェック（指定された終了納品日異常の場合、終了）
     IF       PARA-IN-ENOUDT  <  DEN-F112
              MOVE     "END"        TO  END-FLG
              GO                    TO  SHTDENLJ-READ-EXIT
     END-IF.
*
     IF       PARA-IN-SNOUDT  <=  DEN-F112
     AND      PARA-IN-ENOUDT  >=  DEN-F112
              CONTINUE
     ELSE
              GO           TO  SHTDENLJ-READ-SEC
     END-IF.
 SHTDENLJ-READ-04.
*    倉庫ＣＤチェック
     IF       PARA-IN-SOKO   =  SPACE
              CONTINUE
     ELSE
              IF   PARA-IN-SOKO  =  DEN-F48
                   CONTINUE
              ELSE
                   GO           TO  SHTDENLJ-READ-SEC
              END-IF
     END-IF.
*
 SHTDENLJ-READ-05.
*    数量チェック
     IF       DEN-F15    NOT =  ZERO
              CONTINUE
     ELSE
              GO         TO  SHTDENLJ-READ-SEC
     END-IF.
******
*****付番区分チェック（付番区分＝９：確定の場合）
*****IF       DEN-F276   =  9
*             CONTINUE
*    ELSE
*             GO         TO  SHTDENLJ-READ-SEC
*****END-IF.
*
 SHTDENLJ-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
 MAIN001.
*    基本情報ファイル検索
     MOVE     DEN-F46               TO  JOH-F15.
     MOVE     DEN-F47               TO  JOH-F16.
     MOVE     DEN-F01               TO  JOH-F17.
     MOVE     DEN-F111              TO  JOH-F03.
     MOVE     DEN-F02               TO  JOH-F18.
     MOVE     DEN-F03               TO  JOH-F19.
     PERFORM  YODJOHL2-READ-SEC.
     IF       YODJOHL2-INV-FLG      =   "INV"
              MOVE    "END"         TO   END-FLG
*             DISPLAY NC"基本情報が存在しません" UPON CONS
*             DISPLAY NC"　受信日＝" DEN-F46  UPON CONS
*             DISPLAY NC"　受信時＝" DEN-F47  UPON CONS
*             DISPLAY NC"　取引先＝" DEN-F01  UPON CONS
*             DISPLAY NC"　発注日＝" DEN-F111 UPON CONS
*             DISPLAY NC"　伝票　＝" DEN-F02  UPON CONS
*             DISPLAY NC"　行　　＝" DEN-F03  UPON CONS
*             STOP RUN
              GO             TO   MAIN-EXIT
     END-IF.
*
*既に通知データ作成済の場合は出荷通知データ作成をスキップする。
     IF  JOH-F26 NOT = ZERO  OR
         JOH-F34 NOT = ZERO
         GO                       TO   MAIN003
     END-IF.
 MAIN002.
*    データ作成
     PERFORM  YODSNDL3-WRITE-SEC.
*--  PERFORM  YODJOHL2-REWRITE-SEC.
*
 MAIN003.
*    売上伝票ファイル読込み
     PERFORM  SHTDENLJ-READ-SEC.
     IF       END-FLG  =    "END"
              GO             TO   MAIN-EXIT
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　ヨドバシ　　出荷通知データ抽出処理
****************************************************************
 YODSNDL3-WRITE-SEC     SECTION.
*
     MOVE     "YODSNDL3-WRITE-SEC"  TO   S-NAME.
*
     MOVE      SPACE                TO   SND-REC.
     INITIALIZE                          SND-REC.
*
*  納入予定日付
     MOVE      DEN-F112             TO   SND-F02.
*
*  明細行番号
     MOVE      JOH-F07              TO   SND-F03.
*
*  品目番号
     MOVE      JOH-F08              TO   SND-F04.
*
*  数量
     MOVE      DEN-F15              TO   SND-F05.
*
*  発注伝票日付
     MOVE      DEN-F111             TO   SND-F06.
*
*  参照伝票番号
     MOVE      JOH-F02              TO   SND-F07.
*
*  参照行番号
     MOVE      JOH-F07              TO   SND-F08.
*  配送場所
     MOVE      JOH-F12              TO   SND-F09.
*  出荷先コード
     MOVE      JOH-F06              TO   SND-F10.
*  バッチ（日）
     MOVE      DEN-F46              TO   SND-F11.
*  バッチ（時）
     MOVE      DEN-F47              TO   SND-F12.
*  バッチ（取）
     MOVE      DEN-F01              TO   SND-F13.
*  基幹伝票番号
     MOVE      DEN-F02              TO   SND-F14.
*  基幹行番号
     MOVE      DEN-F03              TO   SND-F15.
*
*  振分倉庫
     MOVE      DEN-F48              TO   SND-F16.
*  出荷通知番号
     MOVE      ZERO                 TO   SND-F17.
*  倉庫確定日
     MOVE      ZERO                 TO   SND-F18.
*  倉庫確定時刻
     MOVE      ZERO                 TO   SND-F19.
*  出荷通知データ作成日
     MOVE      ZERO                 TO   SND-F20.
*  出荷通知データ作成時刻
     MOVE      ZERO                 TO   SND-F21.
*  商品名
     MOVE      JOH-F38              TO   SND-F23.
     WRITE     SND-REC.
     ADD       1                    TO   WRT-CNT.
*
 YODSNDL3-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　基本情報ファイル検索　　　　　　　　　　　　　　*
****************************************************************
 YODJOHL2-READ-SEC    SECTION.
*
     MOVE    "YODJOHL2-READ-SEC"    TO  S-NAME.
*
     MOVE    "   "                  TO  YODJOHL2-INV-FLG.
     READ     YODJOHL2
              INVALID
                  MOVE     "INV"    TO  YODJOHL2-INV-FLG
                  GO                TO  YODJOHL2-READ-EXIT
              NOT INVALID
                  MOVE     "HIT"    TO  YODJOHL2-INV-FLG
     END-READ.
*
     IF   JOH-F26  NOT =  ZERO
          MOVE    "SKP"             TO  YODJOHL2-INV-FLG
     END-IF.
*
 YODJOHL2-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　基本情報ファイル更新　　　　　　　　　　　　　　*
****************************************************************
*YODJOHL2-REWRITE-SEC    SECTION.
*
*    MOVE    "YODJOHL2-REWRITE-SEC" TO  S-NAME.
*
*    MOVE     SYS-DATEW             TO  JOH-F26.
*    MOVE     WK-TIME(1:6)          TO  JOH-F27.
*    REWRITE  JOH-REC.
*
*YODJOHL2-REWRITE-EXIT.
*    EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     DISPLAY "SHTDENLJ READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "YODSNDL3 WRT  CNT = " WRT-CNT   UPON CONS.
     MOVE     WRT-CNT       TO      PARA-OUT-CNT.
*
     IF       YODJOHL2-INV-FLG      =   "INV"
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
              DISPLAY NC"＃基本情報ファイルなし　　＃" UPON CONS
              DISPLAY NC"＃　バッチ（日）＝" DEN-F46   UPON CONS
              DISPLAY NC"＃　バッチ（時）＝" DEN-F47   UPON CONS
              DISPLAY NC"＃　バッチ（取）＝" DEN-F01   UPON CONS
              DISPLAY NC"＃　発注日　　　＝" DEN-F111  UPON CONS
              DISPLAY NC"＃　伝票番号　　＝" DEN-F02   UPON CONS
              DISPLAY NC"＃　行番号　　　＝" DEN-F03   UPON CONS
              DISPLAY NC"＃＃＃＃＃＃＃＃＃＃＃＃＃＃" UPON CONS
              MOVE    4010          TO  PROGRAM-STATUS
     END-IF.
*
     CLOSE     SHTDENLJ  YODJOHL2  YODSNDL3.
*
     DISPLAY   MSG-END   UPON CONS.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
