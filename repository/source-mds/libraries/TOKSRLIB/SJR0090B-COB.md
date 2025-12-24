# SJR0090B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SJR0090B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹　　　　　　　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領データ共通変換                *
*                            　ダイユーエイト(受領・返品）
*    作成日　　　　　　　：　2017/09/04 INOUE                  *
*    処理概要　　　　　　：　各社毎の受領返品データを、　　　  *
*                            共通データに変換する。　　　　　　*
*                                                              *
*    更新日　　　　　　　：　2018/09/20 NAV TAKAHASHI          *
*    更新内容　　　　　　：　リック追加　　　　　　　　　　　　*
*                            　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR0090B.
 AUTHOR.                NAV-ASSIST.
 DATE-WRITTEN.          2017/09/04.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領データファイル
     SELECT   DYJFILE   ASSIGN    TO        DA-01-S-DYJFILE
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    ONL-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
*店舗マスタ
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TEN-F52  TEN-F011
                        FILE  STATUS   IS   TEN-STATUS.
*取引先マスタ
     SELECT   HTOKMS    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*商品変換テーブル
     SELECT   HSHOTBL   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*伝票区分変換マスタ
     SELECT   DENHENF   ASSIGN    TO        DA-01-VI-DENHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       HEN-F01 HEN-F02
                        FILE  STATUS   IS   HEN-STATUS.
*受領受信状況ファイル
     SELECT   COMJYOF   ASSIGN    TO        DA-01-VI-COMJYOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                                            JYO-F03   JYO-F04
                        FILE STATUS    IS   JYO-STATUS.
*受領返品累積ファイル
     SELECT   COMRUIF   ASSIGN    TO        DA-01-VI-COMRUIL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUI-F03   RUI-F01
                                            RUI-F02   RUI-F04
                                            RUI-F31
                        FILE  STATUS   IS   RUI-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領データ　
******************************************************************
 FD  DYJFILE           BLOCK CONTAINS  7   RECORDS
                        LABEL RECORD   IS   STANDARD.
 01  DEN-REC.
     03  DEN-01                   PIC  X(02).
     03  DEN-02                   PIC  X(576).
*    COPY     XXXXXXXX  OF    XFDLIB
*             JOINING   ONL   PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF    XFDLIB
              JOINING   TEN   PREFIX.
******************************************************************
*    得意先マスタ
******************************************************************
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF    XFDLIB
              JOINING   TOK   PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  HSHOTBL            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF    XFDLIB
              JOINING   TBL   PREFIX.
******************************************************************
*    伝票区分変換マスタマスタ
******************************************************************
 FD  DENHENF            LABEL RECORD   IS   STANDARD.
     COPY     DENHENF   OF    XFDLIB
              JOINING   HEN   PREFIX.
******************************************************************
*    受領受信状況ファイル
******************************************************************
 FD  COMJYOF            LABEL RECORD   IS   STANDARD.
     COPY     COMJYOF   OF    XFDLIB
              JOINING   JYO   PREFIX.
******************************************************************
*    受領返品累積ファイル
******************************************************************
 FD  COMRUIF            LABEL RECORD   IS   STANDARD.
     COPY     COMRUIF   OF    XFDLIB
              JOINING   RUI   PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*受領データ（ヘッダ）
     COPY     DYJURHDF  OF   XFDLIB   JOINING   JHD    PREFIX.
*受領データ（明細）
     COPY     DYJURMEF  OF   XFDLIB   JOINING   JME    PREFIX.
*
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*伝票番号ブレイク判定用
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
*取引先ブレイク判定用
 01  WK-TORI                 PIC  9(08)     VALUE  ZERO.
*エラーチェック用
 01  WK-ERR-KEIJOUBI         PIC  X(01)     VALUE  SPACE.
 01  WK-ERR-GENTANKA         PIC  X(01)     VALUE  SPACE.
*カウント
 01  WK-CNT-HEN              PIC  9(07)     VALUE  ZERO.
 01  WK-CNT-JYR              PIC  9(07)     VALUE  ZERO.
 01  WK-DEN-HEN              PIC  9(07)     VALUE  ZERO.
 01  WK-DEN-JYR              PIC  9(07)     VALUE  ZERO.
*カウント累計
 01  SUM-CNT-HEN             PIC  9(07)     VALUE  ZERO.
 01  SUM-CNT-JYR             PIC  9(07)     VALUE  ZERO.
 01  SUM-DEN-HEN             PIC  9(07)     VALUE  ZERO.
 01  SUM-DEN-JYR             PIC  9(07)     VALUE  ZERO.
*退避
 01  BK-JYO-F09              PIC  9(05)     VALUE  ZERO.
 01  BK-JYO-F10              PIC  9(05)     VALUE  ZERO.
*
 01  STR-TIME                PIC  9(04)     VALUE  ZERO.
*
 01  WK-DENKU                PIC  X(02)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  DENHENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  COMJYOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  COMRUIF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*
 01  FLG-SKIP                PIC  X(04)     VALUE  SPACE.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  ONL-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
*    03  MEI-STATUS        PIC  X(02).
     03  JYO-STATUS        PIC  X(02).
     03  RUI-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR0090B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0090B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR0090B".
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
*時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
*
 01  WK-HEN-TOKCD            PIC  X(06).
 01  FILLER                  REDEFINES   WK-HEN-TOKCD.
     03  WK-TOKCD-HEN        PIC  9(06).
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
*01  PARA-TORI              PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME.
*                                      PARA-TORI.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYJFILE.
     MOVE      "DYJFILE"   TO   AB-FILE.
     MOVE      ONL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTENMS.
     MOVE      "TENMS1"     TO   AB-FILE.
     MOVE      TEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HTOKMS.
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
                        PROCEDURE   HSHOTBL.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DENHENF.
     MOVE      "DENHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMJYOF.
     MOVE      "COMJYOL1"   TO   AB-FILE.
     MOVE      JYO-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRUIF.
     MOVE      "COMRUIL1"   TO   AB-FILE.
     MOVE      RUI-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     DYJFILE
                        HTENMS    HSHOTBL
                        HTOKMS    DENHENF.
     OPEN     I-O       COMRUIF.
     OPEN     I-O       COMJYOF.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*    MOVE     SPACE     TO        WK-HEAD-REC.
*    INITIALIZE                   WK-HEAD-REC.
*    MOVE     SPACE     TO        WK-MEISAI-REC.
*    INITIALIZE                   WK-MEISAI-REC.
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
*システム時間取得
     ACCEPT    WK-TIME          FROM   TIME.
     MOVE      WK-TIME(1:4)     TO     STR-TIME.
*データ初期READ
 INIT-01.
     READ     DYJFILE
              AT END    MOVE      9         TO  END-FG
              DISPLAY NC"＃＃対象データ無！！＃＃" UPON CONS
              GO              TO    INIT-EXIT
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*PARA-IN取引先のみ対象
*    IF       ONL-F013  =     PARA-TORI
*             CONTINUE
*    ELSE
*             GO              TO    INIT-01
*    END-IF.
*
*件数カウント
*    １件目ヘッダ以外読み飛ばし
     IF    DEN-01   =  "HD"
           MOVE      SPACE         TO    JHD-REC
           INITIALIZE                    JHD-REC
           MOVE      DEN-REC       TO    JHD-REC
     ELSE
           GO                      TO    INIT-01
     END-IF.
*    対象外伝票区分
*　  伝票区分変換マスタチェック
     EVALUATE JHD-H12(1:4)
       WHEN   "4950"
              MOVE   1225    TO        HEN-F01
       WHEN   "4959"
              MOVE   1225    TO        HEN-F01
       WHEN   "4953"
              MOVE  12252    TO        HEN-F01
*#2018/09/20 NAV ST リック追加
       WHEN   "4978"
              MOVE  JHD-H04(1:6)  TO   WK-HEN-TOKCD
              MOVE  WK-TOKCD-HEN  TO   HEN-F01
*#2018/09/20 NAV ED リック追加
     END-EVALUATE.
     MOVE     JHD-H10        TO        HEN-F02.
     PERFORM  DENHEN-READ-SEC.
     IF       DENHENF-INV-FLG  NOT =  "INV"
         IF     HEN-F05  =   "2"
                GO       TO     INIT-01
         END-IF
     ELSE
                GO       TO     INIT-01
     END-IF.
*
*    返品
       IF   ( JHD-H10 =    "51" )
*---          レコード件数カウントアップ
*---          ADD       1            TO    WK-CNT-HEN
*             伝票枚数カウントアップ
              IF   JHD-H03           NOT = WK-DENNO
                   ADD  1            TO    WK-DEN-HEN
                   MOVE JHD-H03      TO    WK-DENNO
              END-IF
       END-IF.
*    受領
       IF   ( JHD-H10 NOT =    "51" )
*---          レコード件数カウントアップ
*---          ADD       1            TO    WK-CNT-JYR
*             伝票枚数カウントアップ
              IF   JHD-H03           NOT = WK-DENNO
                   ADD   1           TO    WK-DEN-JYR
                   MOVE JHD-H03      TO    WK-DENNO
              END-IF
       END-IF.
*
*      取引先コード保管
       MOVE   HEN-F01          TO    WK-TORI.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*        　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*
 MAIN-01.
*ヘッダ処理
*    IF    DEN-01   =  "HD"
*          MOVE      SPACE         TO    JHD-REC
*          INITIALIZE                    JHD-REC
*          MOVE      DEN-REC       TO    JHD-REC
*    END-IF.
*明細処理
     IF    DEN-01   =  "DT"
           MOVE      DEN-REC       TO    JME-REC
           PERFORM   EDIT-SEC
     END-IF.
*
*    PERFORM  EDIT-SEC.
*
 MAIN-02.
     READ     DYJFILE
                AT END
                           MOVE      9         TO  END-FG
                           GO                  TO  MAIN-EXIT
                NOT AT END
                           ADD       1         TO  RD-CNT
     END-READ.
*PARA-IN取引先のみ対象
*    IF       ONL-F013  =     JHD-H04(1:6)
*             CONTINUE
*    ELSE
*             GO              TO    MAIN-02
*    END-IF.
*
*件数カウント
*   対象外レコード区分
     IF  ( DEN-01   NOT =  "HD" ) AND
         ( DEN-01   NOT =  "DT" )
           GO                      TO    MAIN-02
     END-IF.
*   ヘッダ保管
     IF    DEN-01   =  "HD"
           MOVE      SPACE         TO    JHD-REC
           INITIALIZE                    JHD-REC
           MOVE      DEN-REC       TO    JHD-REC
*          対象外伝票区分
*　        伝票区分変換マスタチェック
           EVALUATE JHD-H12(1:4)
             WHEN   "4950"
                    MOVE   1225    TO        HEN-F01
             WHEN   "4959"
                    MOVE   1225    TO        HEN-F01
             WHEN   "4953"
                    MOVE  12252    TO        HEN-F01
*#2018/09/20 NAV ST リック追加
             WHEN   "4978"
                    MOVE  JHD-H04(1:6)  TO   WK-HEN-TOKCD
                    MOVE  WK-TOKCD-HEN  TO   HEN-F01
*#2018/09/20 NAV ED リック追加
           END-EVALUATE
           MOVE     JHD-H10        TO        HEN-F02
           PERFORM  DENHEN-READ-SEC
           IF       DENHENF-INV-FLG  NOT =  "INV"
               IF     HEN-F05  =   "2"
                      MOVE     "SKIP" TO     FLG-SKIP
                      GO       TO     MAIN-02
               ELSE
                      MOVE     "    " TO     FLG-SKIP
               END-IF
           ELSE
                      MOVE     "SKIP" TO     FLG-SKIP
                      GO       TO     MAIN-02
           END-IF
*
*---       IF   ( JHD-H10 =    "55" OR "56" OR "57" )
*---              GO       TO     MAIN-02
*---       END-IF
*
*          取引先ブレイクで状況ファイル出力
           IF       HEN-F01   NOT =  WK-TORI
*                  受領受信状況ファイル出力
                    PERFORM             COMJYOF-WRT-SEC
*                  取引先コード保管
                    MOVE   HEN-F01      TO   WK-TORI
           END-IF
     END-IF.
*
*   明細スキップ(対象外伝票区分）
     IF    DEN-01   =  "DT"
           IF       FLG-SKIP   =   "SKIP"
                    GO            TO   MAIN-02
     END-IF.
*
*   返品伝票カウント
     IF   ( JHD-H10     =    "51" ) AND
          ( DEN-01      =    "DT" )
*           レコード件数カウントアップ
            ADD       1            TO    WK-CNT-HEN
*           伝票枚数カウントアップ
            IF   JHD-H03           NOT = WK-DENNO
                 ADD  1            TO    WK-DEN-HEN
                 MOVE JHD-H03      TO    WK-DENNO
            END-IF
     END-IF.
*   受領伝票カウント
     IF   ( JHD-H10 NOT =    "51" ) AND
          ( DEN-01      =    "DT" )
*           レコード件数カウントアップ
            ADD       1            TO    WK-CNT-JYR
*           伝票枚数カウントアップ
            IF   JHD-H03           NOT = WK-DENNO
                 ADD  1            TO    WK-DEN-JYR
                 MOVE JHD-H03      TO    WK-DENNO
            END-IF
     END-IF.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*          　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*
     MOVE     SPACE         TO        RUI-REC.
     INITIALIZE                       RUI-REC.
*    計上日(IN:8桁)
     IF  JHD-H06 NUMERIC
         MOVE        "2"            TO    LINK-IN-KBN
         MOVE        JHD-H06        TO    LINK-IN-YMD8
         CALL        "SKYDTCKB"  USING    LINK-IN-KBN
                                          LINK-IN-YMD6
                                          LINK-IN-YMD8
                                          LINK-OUT-RET
                                          LINK-OUT-YMD8
         IF          LINK-OUT-RET   =     ZERO
             MOVE    JHD-H06        TO    RUI-F08
             MOVE    " "            TO    WK-ERR-KEIJOUBI
         ELSE
             MOVE    JHD-H06        TO    RUI-F08
             MOVE    "1"            TO    WK-ERR-KEIJOUBI
         END-IF
     ELSE
         MOVE        ZERO           TO    RUI-F08
         MOVE        "1"            TO    WK-ERR-KEIJOUBI
     END-IF.
*取引先マスタ索引
     EVALUATE JHD-H12(1:4)
       WHEN   "4950"
              MOVE   1225   TO  TOK-F01 TEN-F52 TBL-F01 RUI-F03
       WHEN   "4959"
              MOVE   1225   TO  TOK-F01 TEN-F52 TBL-F01 RUI-F03
       WHEN   "4953"
              MOVE  12252   TO  TOK-F01 TEN-F52 TBL-F01 RUI-F03
*#2018/09/20 NAV ST リック追加
       WHEN   "4978"
              MOVE  JHD-H04(1:6)  TO   WK-HEN-TOKCD
              MOVE  WK-TOKCD-HEN  TO
                                  TOK-F01 TEN-F52 TBL-F01 RUI-F03
*#2018/09/20 NAV ED リック追加
     END-EVALUATE.
*--- MOVE     JHD-H04(1:6)      TO        TOK-F01.
     PERFORM  HTOKMS-READ-SEC.
*店舗マスタ索引
*--- MOVE     JHD-H04(1:6)      TO        TEN-F52.
     MOVE     JHD-H17(9:5)      TO        TEN-F011.
     PERFORM  HTENMS-READ-SEC.
*商品変換ＴＢＬ索引
*--- MOVE     JHD-H04(1:6)   TO        TBL-F01.
     MOVE     JME-M04(1:13)  TO        TBL-F02.
     PERFORM  HSHOTBL-READ-SEC.
*原単価
     IF     ( JME-M10 NOT NUMERIC ) OR
            ( JME-M10 =   ZERO    )
              MOVE   "1"     TO        WK-ERR-GENTANKA
              MOVE   ZERO    TO        RUI-F35
     ELSE
              MOVE   " "     TO        WK-ERR-GENTANKA
     END-IF.
*バッチ日付
     MOVE     PARA-JDATE     TO        RUI-F01.
*バッチ時刻
     MOVE     PARA-JTIME     TO        RUI-F02.
*バッチ取引先
*--- MOVE     JHD-H04(1:6)   TO        RUI-F03.
*伝票番号
     MOVE     JHD-H03      TO        RUI-F04.
*元伝票番号
*    F05 : INIT
*相手伝票区分
     MOVE     JHD-H10      TO        RUI-F06.
*店舗コード
     MOVE     JHD-H17(9:5) TO        RUI-F07.
*計上日
*分類コード
     MOVE     JHD-H18      TO        RUI-F09.
*訂正区分（Ｈ）
*    F10 : INIT
*直接納品先コード
*    F11 : INIT
*コメント
*    F12 : INIT
*行番号
     MOVE     JME-M02        TO        RUI-F31.
*赤黒区分
     MOVE     ZERO           TO        RUI-F32.
*相手商品コード
     MOVE     JME-M04(1:13)  TO        RUI-F33.
*数量
*    MOVE     JME-M08        TO        RUI-F34.
     COMPUTE  RUI-F34   =    JME-M08   /   10.
*原単価
     IF       WK-ERR-GENTANKA  NOT =  "1"
*             MOVE JME-M10   TO        RUI-F35
              COMPUTE   RUI-F35    =   JME-M10   /  100
     END-IF.
*原価金額
     MOVE     JME-M09        TO        RUI-F36.
*    COMPUTE  RUI-F36   =    ONL-F316  /   100.
*商品名カナ１
     MOVE     JME-M06        TO      RUI-F37.
*商品名カナ２
     MOVE     SPACE         TO      RUI-F38.
*返品理由
*    F39 : INIT
*訂正区分（Ｍ）
*    F40 : INIT
*売単価
     MOVE     JME-M11        TO        RUI-F41.
*    COMPUTE  RUI-F41   =    JME-M11   /   100.
*自社伝票区分
     MOVE     HEN-F04        TO        RUI-F51.
*
*サカタ商品コード
     IF   HSHOTBL-INV-FLG    NOT =    "INV"
          MOVE    TBL-F031   TO        RUI-F52
          MOVE    TBL-F0321  TO        RUI-F53
          MOVE    TBL-F0322  TO        RUI-F54
          MOVE    TBL-F0323  TO        RUI-F55
     END-IF.
*    エラー区分１（取引先マスタ）
     IF   HTOKMS-INV-FLG  =  "INV"
          MOVE  "1"          TO        RUI-F561
     END-IF.
*    エラー区分２（店舗マスタ）
     IF   HTENMS-INV-FLG  =  "INV"
          MOVE  "1"          TO        RUI-F562
     END-IF.
*    エラー区分３（計上日）
     IF   WK-ERR-KEIJOUBI  = "1"
          MOVE  "1"          TO        RUI-F563
     END-IF.
*    エラー区分４（原単価）
     IF   WK-ERR-GENTANKA  = "1"
          MOVE  "1"          TO        RUI-F564
     END-IF.
*    エラー区分５（商品変換ＴＢＬ）
     IF   HSHOTBL-INV-FLG  = "INV"
          MOVE  "1"          TO        RUI-F565
     END-IF.
*    エラー区分６（非使用）
*    F566 : INIT
*    エラー区分７（非使用）
*    F567 : INIT
*    エラー区分８（非使用）
*    F568 : INIT
*    エラー区分９（非使用）
*    F569 : INIT
*    エラー区分10（非使用）
*    F56A : INIT
*    返品累積データ作成区分
*    F57  : INIT
*    返品累積データ作成日
*    F58  : INIT
*受領返品累積ファイル出力
     WRITE  RUI-REC.
     ADD    1              TO   WRT-CNT .
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     IF        WRT-CNT   >   ZERO
*              受領受信状況ファイル出力
               PERFORM   COMJYOF-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   NC"うち受領＝" SUM-CNT-JYR NC"件" UPON CONS.
     DISPLAY   NC"　　　　　" SUM-DEN-JYR NC"枚" UPON CONS.
     DISPLAY   NC"うち返品＝" SUM-CNT-HEN NC"件" UPON CONS.
     DISPLAY   NC"　　　　　" SUM-DEN-HEN NC"枚" UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     DYJFILE  HTENMS   HSHOTBL  DENHENF
               HTOKMS   COMJYOF  COMRUIF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　受領受信状況ファイル出力
****************************************************************
 COMJYOF-WRT-SEC         SECTION.
*
     MOVE   "COMJYOF-WRT-SEC"   TO   S-NAME.
*
     ACCEPT  WK-TIME       FROM      TIME.
*
 COMJYOF-WRT-HEN.
* 返品
     IF      WK-CNT-HEN =  ZERO
             GO            TO        COMJYOF-WRT-JYR
     END-IF.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
*    EVALUATE JHD-H12(1:4)
*      WHEN   "4950"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4959"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4953"
*             MOVE       12252   TO  JYO-F03
*    END-EVALUATE.
     MOVE    WK-TORI       TO        JYO-F03.
     MOVE    02            TO        JYO-F04.
     READ    COMJYOF
       INVALID
         CONTINUE
       NOT INVALID
         MOVE     JYO-F09  TO        BK-JYO-F09
         MOVE     JYO-F10  TO        BK-JYO-F10
         DELETE   COMJYOF
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
*日付
     MOVE    PARA-JDATE    TO        JYO-F01.
*時刻
     MOVE    PARA-JTIME    TO        JYO-F02.
*取引先
*    EVALUATE JHD-H12(1:4)
*      WHEN   "4950"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4959"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4953"
*             MOVE       12252   TO  JYO-F03
*    END-EVALUATE.
     MOVE    WK-TORI       TO        JYO-F03.
*データ種別
     MOVE    02            TO        JYO-F04.
*受信開始時刻（非使用）
*    INIT
*受信終了時刻（非使用）
*    INIT
*データ変換開始時刻
     MOVE    STR-TIME      TO        JYO-F07.
*データ変換終了時刻
     MOVE    WK-TIME(1:4)  TO        JYO-F08.
*受信件数・伝票枚数
     ADD     WK-CNT-HEN    TO        SUM-CNT-HEN.
     ADD     WK-DEN-HEN    TO        SUM-DEN-HEN.
     ADD     BK-JYO-F09    TO        WK-CNT-HEN.
     ADD     BK-JYO-F10    TO        WK-DEN-HEN.
     MOVE    WK-CNT-HEN    TO        JYO-F09.
     MOVE    WK-DEN-HEN    TO        JYO-F10.
     MOVE    0             TO        WK-CNT-HEN BK-JYO-F09.
     MOVE    0             TO        WK-DEN-HEN BK-JYO-F10.
*受信状況フラグ
*    INIT
*変換状況フラグ
*    INIT
*更新状況フラグ
*    INIT
*実行完了ＦＬＧ
*    INIT
*結果ＦＬＧ
*    INIT
*
     WRITE   JYO-REC.
*
 COMJYOF-WRT-JYR.
* 受領
     IF      WK-CNT-JYR =  ZERO
             GO            TO        COMJYOF-WRT-EXIT
     END-IF.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
     MOVE    PARA-JDATE    TO        JYO-F01.
     MOVE    PARA-JTIME    TO        JYO-F02.
*    EVALUATE JHD-H12(1:4)
*      WHEN   "4950"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4959"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4953"
*             MOVE       12252   TO  JYO-F03
*    END-EVALUATE.
     MOVE    WK-TORI       TO        JYO-F03.
     MOVE    01            TO        JYO-F04.
     READ    COMJYOF
       INVALID
         CONTINUE
       NOT INVALID
         MOVE     JYO-F09  TO        BK-JYO-F09
         MOVE     JYO-F10  TO        BK-JYO-F10
         DELETE   COMJYOF
     END-READ.
*
     MOVE    SPACE         TO        JYO-REC.
     INITIALIZE                      JYO-REC.
*日付
     MOVE    PARA-JDATE    TO        JYO-F01.
*時刻
     MOVE    PARA-JTIME    TO        JYO-F02.
*取引先
*    EVALUATE JHD-H12(1:4)
*      WHEN   "4950"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4959"
*             MOVE        1225   TO  JYO-F03
*      WHEN   "4953"
*             MOVE       12252   TO  JYO-F03
*    END-EVALUATE.
     MOVE    WK-TORI       TO        JYO-F03.
*データ種別
     MOVE    01            TO        JYO-F04.
*受信開始時刻（非使用）
*    INIT
*受信終了時刻（非使用）
*    INIT
*データ変換開始時刻
     MOVE    STR-TIME      TO        JYO-F07.
*データ変換終了時刻
     MOVE    WK-TIME(1:4)  TO        JYO-F08.
*受信件数・伝票枚数
     ADD     WK-CNT-JYR    TO        SUM-CNT-JYR.
     ADD     WK-DEN-JYR    TO        SUM-DEN-JYR.
     ADD     BK-JYO-F09    TO        WK-CNT-JYR.
     ADD     BK-JYO-F10    TO        WK-DEN-JYR.
     MOVE    WK-CNT-JYR    TO        JYO-F09.
     MOVE    WK-DEN-JYR    TO        JYO-F10.
     MOVE    0             TO        WK-CNT-JYR BK-JYO-F09.
     MOVE    0             TO        WK-DEN-JYR BK-JYO-F10.
*受信状況フラグ
*    INIT
*変換状況フラグ
*    INIT
*更新状況フラグ
*    INIT
*実行完了ＦＬＧ
*    INIT
*結果ＦＬＧ
*    INIT
*
     WRITE   JYO-REC.
*
     GO      TO   COMJYOF-WRT-EXIT.
*
 COMJYOF-WRT-EXIT.
     EXIT.
****************************************************************
*　　取引先マスタ読込
****************************************************************
 HTOKMS-READ-SEC         SECTION.
*
     MOVE   "HTOKMS-READ-SEC"   TO   S-NAME.
*
     READ  HTOKMS
           INVALID     MOVE  "INV"  TO  HTOKMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ読込
****************************************************************
 HTENMS-READ-SEC         SECTION.
*
     MOVE   "HTENMS-READ-SEC"   TO   S-NAME.
*
     READ  HTENMS
           INVALID     MOVE  "INV"  TO  HTENMS-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HTENMS-INV-FLG
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
****************************************************************
*　　商品変換ＴＢＬ読込
****************************************************************
 HSHOTBL-READ-SEC        SECTION.
*
     MOVE   "HSHOTBL-READ-SEC"  TO   S-NAME.
*
     READ  HSHOTBL
           INVALID     MOVE  "INV"  TO  HSHOTBL-INV-FLG
           NOT INVALID MOVE  SPACE  TO  HSHOTBL-INV-FLG
     END-READ.
*
 HSHOTBL-READ-EXIT.
     EXIT.
****************************************************************
*　　伝票区分変換マスタ読込
****************************************************************
 DENHEN-READ-SEC         SECTION.
*
     MOVE   "DENHEN-READ-SEC"   TO   S-NAME.
*
     READ  DENHENF
           INVALID     MOVE  "INV"  TO  DENHENF-INV-FLG
           NOT INVALID MOVE  SPACE  TO  DENHENF-INV-FLG
     END-READ.
*
 DENHEN-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
