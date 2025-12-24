****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    業務名　　　　　　　：　ダイユーエイト新ＥＤＩシステム　　*
*    モジュール名　　　　：　納品予定データ送信形式変換        *
*    作成日／更新日　　　：　2010/10/25                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　受け取ったパラメタより対象データ  *
*                            を出荷情報保存データより抽出する。*
*    作成日／更新日　　　：　2013/09/10                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　日敷を追加　　　　　　　　　　　  *
*    作成日／更新日　　　：　2018/01/29                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　リック資材／植物追加　　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY7313C.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*出荷情報データ
     SELECT   DYSYUKF   ASSIGN    TO        DA-01-VI-DYSYUKL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       DYS-F01   DYS-F02
                                            DYS-F03   DYS-F04
                                            DYS-F08   DYS-F06
                                            DYS-F07
                        FILE      STATUS    DYS-STATUS.
*基本情報データ
     SELECT   DYJOHOF   ASSIGN    TO        DA-01-VI-DYJOHOL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       DYJ-F01   DYJ-F02
                                            DYJ-F03   DYJ-F04
                                            DYJ-F05   DYJ-F06
                                            DYJ-F07   DYJ-F08
                        FILE      STATUS    DYJ-STATUS.
*----<< 店舗マスタ >>-*
     SELECT   HTENMS    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      DYNAMIC
                        RECORD    KEY       TEN-F52   TEN-F011
                        FILE      STATUS    TEN-STATUS.
*出荷送信データ
     SELECT   SNDRIC  ASSIGN    TO        DA-01-S-SNDDYDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SND-STATUS.
*出荷送信データ
     SELECT   SNDEFM  ASSIGN    TO        DA-01-S-SNDEFDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SEF-STATUS.
*出荷送信データ
     SELECT   SNDMAX  ASSIGN    TO        DA-01-S-SNDMXDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SMX-STATUS.
*出荷送信データ（日敷） 2013.09.10
     SELECT   SNDNIS  ASSIGN    TO        DA-01-S-SNDNSDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SNS-STATUS.
*#2018/01/29 NAV ST
*出荷送信データ（リック資材）
     SELECT   SNDYRZ  ASSIGN    TO        DA-01-S-SNDRZDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SRZ-STATUS.
*出荷送信データ（リック植物）
     SELECT   SNDYRS  ASSIGN    TO        DA-01-S-SNDRSDT
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SRS-STATUS.
*#2018/01/29 NAV ED
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    出荷情報データ
******************************************************************
 FD  DYSYUKF            LABEL RECORD   IS   STANDARD.
     COPY     DYSYUKF   OF        XFDLIB
              JOINING   DYS       PREFIX.
******************************************************************
*    基本情報データ
******************************************************************
 FD  DYJOHOF            LABEL RECORD   IS   STANDARD.
     COPY     DYJOHOF   OF        XFDLIB
              JOINING   DYJ       PREFIX.
******************************************************************
*    店舗マスタ
******************************************************************
 FD  HTENMS             BLOCK     CONTAINS   8   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HTENMS    OF   XFDLIB    JOINING   TEN  AS   PREFIX.
******************************************************************
*    出荷送信データ
******************************************************************
 FD  SNDRIC
                        BLOCK CONTAINS 1 RECORDS.
 01  SND-REC            PIC  X(3141).
******************************************************************
*    出荷送信データ
******************************************************************
 FD  SNDEFM
                        BLOCK CONTAINS 1 RECORDS.
 01  SEF-REC            PIC  X(3141).
******************************************************************
*    出荷送信データ
******************************************************************
 FD  SNDMAX
                        BLOCK CONTAINS 1 RECORDS.
 01  SMX-REC            PIC  X(3141).
******************************************************************
*    出荷送信データ（日敷）2013.09.10
******************************************************************
 FD  SNDNIS
                        BLOCK CONTAINS 1 RECORDS.
 01  SNS-REC            PIC  X(3141).
*
*#2018/01/29 NAV ST
******************************************************************
*    出荷送信データ（リック資材）
******************************************************************
 FD  SNDYRZ
                        BLOCK CONTAINS 1 RECORDS.
 01  SRZ-REC            PIC  X(3141).
******************************************************************
*    出荷送信データ（リック植物）
******************************************************************
 FD  SNDYRS
                        BLOCK CONTAINS 1 RECORDS.
 01  SRS-REC            PIC  X(3141).
*#2018/01/29 NAV ED
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  DYJOHOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-CNT.
     03  READ-CNT            PIC  9(08)     VALUE  ZERO.
     03  SKIP-CNT            PIC  9(08)     VALUE  ZERO.
     03  SND-CNT1            PIC  9(07)     VALUE  ZERO.
     03  SND-CNT2            PIC  9(07)     VALUE  ZERO.
     03  SND-CNT3            PIC  9(07)     VALUE  ZERO.
*2013.09.10↓
     03  SND-CNT4            PIC  9(07)     VALUE  ZERO.
*2013.09.10↑
*#2018/01/29 NAV ST
     03  SND-CNT5            PIC  9(07)     VALUE  ZERO.
     03  SND-CNT6            PIC  9(07)     VALUE  ZERO.
*#2018/01/29 NAV ED
 01  WK-DENNO                PIC  9(09)     VALUE  ZERO.
 01  WK-TAIL-02              PIC  9(11)     VALUE  ZERO.
 01  WK-TAIL-03              PIC  9(11)     VALUE  ZERO.
 01  WK-TORICD               PIC  9(08)     VALUE  ZERO.
 01  WK-TENCD                PIC  9(05)     VALUE  ZERO.
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DYS-STATUS        PIC  X(02).
     03  DYJ-STATUS        PIC  X(02).
     03  TEN-STATUS        PIC  X(02).
     03  SND-STATUS        PIC  X(02).
     03  SEF-STATUS        PIC  X(02).
     03  SMX-STATUS        PIC  X(02).
*2013.09.10↓
     03  SNS-STATUS        PIC  X(02).
*2013.09.10↑
*#2015/01/29 NAV ST
     03  SRZ-STATUS        PIC  X(02).
     03  SRS-STATUS        PIC  X(02).
*#2015/01/29 NAV ED
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY7313C".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7313C".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY7313C".
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
*****03  TAIL-02            PIC   9(11).
*****03  FILLER             PIC   X(11).
*****03  TAIL-03            PIC   9(11).
     03  FILLER             PIC   X(33).
     03  TAIL-04            PIC   X(10).
*
 01  WK-NOUHIN-SU           PIC   9(08).
 01  WK-NOUHIN-SU-R         REDEFINES  WK-NOUHIN-SU.
     03  WK-NOUHIN-1        PIC   X(01).
     03  WK-NOUHIN-2        PIC   X(06).
     03  WK-NOUHIN-3        PIC   X(01).
*
 01  WK-NOUSU.
     03  WK-NOUSU-1         PIC   X(06).
     03  WK-NOUSU-2         PIC   X(01).
     03  WK-NOUSU-3         PIC   X(01).
*
 01  WK-GENKAX.
     03  WK-GENKA           PIC   9(10)  VALUE  ZERO.
*
 LINKAGE                SECTION.
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(04).
 01  PARA-TORICD            PIC   9(08).
 01  PARA-SOKO              PIC   X(02).
 01  PARA-NOUDT             PIC   9(08).
 01  PARA-SNDCNT1           PIC   9(07).
 01  PARA-SNDCNT2           PIC   9(07).
 01  PARA-SNDCNT3           PIC   9(07).
*2013.09.10↓
 01  PARA-SNDCNT4           PIC   9(07).
*2013.09.10↑
*#2018/01/29 NAV ST
 01  PARA-SNDCNT5           PIC   9(07).
 01  PARA-SNDCNT6           PIC   9(07).
*#2018/01/29 NAV ED
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-JDATE
                                       PARA-JTIME
                                       PARA-TORICD
                                       PARA-SOKO
                                       PARA-NOUDT
                                       PARA-SNDCNT1
                                       PARA-SNDCNT2
                                       PARA-SNDCNT3
*2013.09.10↓
                                       PARA-SNDCNT4
*2013.09.10↑
*#2018/01/29 NAV ST
                                       PARA-SNDCNT5
                                       PARA-SNDCNT6.
*#2018/01/29 NAV ED
*
 DECLARATIVES.
*
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYSYUKF.
     MOVE      "DYSYUKL2"    TO   AB-FILE.
     MOVE      DYS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDRIC.
     MOVE      "SNDDYDT"    TO   AB-FILE.
     MOVE      SND-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DYJOHOF.
     MOVE      "DYJOHOL1"    TO   AB-FILE.
     MOVE      DYJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*2013.09.10↓
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDNIS.
     MOVE      "SNDNSDT "    TO   AB-FILE.
     MOVE      DYJ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*2013.09.10↑
*#2018/01/29 NAV ST
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDYRZ.
     MOVE      "SNDRZDT "    TO   AB-FILE.
     MOVE      SRZ-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SNDYRS.
     MOVE      "SNDRSDT "    TO   AB-FILE.
     MOVE      SRS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*2013.09.10↑
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
     OPEN     I-O       DYSYUKF   DYJOHOF.
     OPEN     EXTEND    SNDRIC    SNDEFM    SNDMAX.
*2013.09.10↓
     OPEN     EXTEND    SNDNIS.
*2013.09.10↑
*#2018/01/29 NAV ST
     OPEN     EXTEND    SNDYRZ    SNDYRS.
*#2018/01/29 NAV ED
     OPEN     INPUT     HTENMS.
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
*    出荷情報スタート
     MOVE     SPACE          TO   DYS-REC.
     INITIALIZE                   DYS-REC.
     MOVE     PARA-JDATE     TO   DYS-F01.
     MOVE     PARA-JTIME     TO   DYS-F02.
     MOVE     PARA-TORICD    TO   DYS-F03.
     MOVE     PARA-SOKO      TO   DYS-F04.
     MOVE     PARA-NOUDT     TO   DYS-F08.
     START    DYSYUKF   KEY  >=   DYS-F01   DYS-F02
                                  DYS-F03   DYS-F04
                                  DYS-F08   DYS-F06
                                  DYS-F07
         INVALID   KEY
              MOVE    "END"  TO   END-FLG
              GO   TO   INIT-EXIT
     END-START.
*    出荷情報データ読込み
     PERFORM DYSYUKF-READ-SEC.
*
     IF   END-FLG  NOT =  SPACE
          MOVE     DYS-F03    TO   WK-TORICD
          MOVE     DYS-F05    TO   WK-TENCD
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷情報データ　ＲＥＡＤ　　　　　　　　　　　　*
****************************************************************
 DYSYUKF-READ-SEC    SECTION.
*
     READ     DYSYUKF
              AT  END
                  MOVE     "END"    TO  END-FLG
                  GO                TO  DYSYUKF-READ-EXIT
              NOT AT END
                  ADD       1       TO  READ-CNT
     END-READ.
*
*    IF    READ-CNT(6:3)  =  "000" OR "500"
*          DISPLAY "READ-CNT = " READ-CNT  UPON CONS
*    END-IF.
*    送信ＦＬＧのチェック
     IF       DYS-F11  =  "1"
              GO                 TO   DYSYUKF-READ-SEC
     END-IF.
*    バッチ番号のチェック
     IF       PARA-JDATE   =  DYS-F01
     AND      PARA-JTIME   =  DYS-F02
     AND      PARA-TORICD  =  DYS-F03
              CONTINUE
     ELSE
              MOVE     "END"        TO  END-FLG
              GO                    TO  DYSYUKF-READ-EXIT
     END-IF.
*倉庫ＣＤチェック
     IF       PARA-SOKO   =  SPACE
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-SOKO   =  DYS-F04
                       CONTINUE
              ELSE
                       GO        TO   DYSYUKF-READ-SEC
              END-IF
     END-IF.
*納品日のチェック
     IF       PARA-NOUDT  =  ZERO
              CONTINUE
     ELSE
*             抽出条件のチェック
              IF       PARA-NOUDT  =  DYS-F08
                       CONTINUE
              ELSE
                       GO        TO   DYSYUKF-READ-SEC
              END-IF
     END-IF.
*
 DYSYUKF-READ-EXIT.
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
*    MOVE     "0"                 TO   DYS-A20(347:1).
*****MOVE     DYS-K20             TO   SND-REC.
     IF       SND-REC(23:1)   =   ZERO
     AND      SND-REC(22:1)   =   SPACE
              MOVE     SPACE      TO   SND-REC(23:1)
     END-IF.
*    ヘッダを書き込むかどうか判断します。
     IF       DYS-F06    =   WK-DENNO
              CONTINUE
     ELSE
**************テイル行出力するか判断
              IF  SND-CNT1  >  ZERO
              OR  SND-CNT2  >  ZERO
              OR  SND-CNT3  >  ZERO
              OR  SND-CNT4  >  ZERO
              OR  SND-CNT5  >  ZERO
              OR  SND-CNT6  >  ZERO
                  MOVE SPACE      TO   SND-REC
                  INITIALIZE           SND-REC
                  MOVE SPACE      TO   WK-TAIL-REC
                  INITIALIZE           WK-TAIL-REC
                  MOVE "TR"       TO   TAIL-01
***************** MOVE WK-TAIL-02 TO   TAIL-02
***************** MOVE WK-TAIL-03 TO   TAIL-03
                  MOVE ALL "0"    TO   TAIL-04
******************MOVE WK-TAIL-REC TO  SND-REC
                  PERFORM  HTENMS-READ-SEC
******************ダイユーエイト／ＭＡＸ出力
                  IF  WK-TORICD = 1225
                      IF  TEN-F14  =  ZERO
                          MOVE WK-TAIL-REC TO  SND-REC
                          WRITE SND-REC
                          ADD   1     TO    SND-CNT1
                      ELSE
                          MOVE WK-TAIL-REC TO  SMX-REC
                          WRITE SMX-REC
                          ADD   1     TO    SND-CNT2
                      END-IF
                  END-IF
******************エイトファーム
                  IF  WK-TORICD = 12251
                      MOVE WK-TAIL-REC TO  SEF-REC
                      WRITE SEF-REC
                      ADD   1         TO    SND-CNT3
                  END-IF
*2013.09.10↓
******************日敷
                  IF  WK-TORICD = 12252
                      MOVE WK-TAIL-REC TO  SNS-REC
                      WRITE SNS-REC
                      ADD   1         TO    SND-CNT4
                  END-IF
*2013.09.10↑
*#2018/01/29 NAV ST↓
******************リック資材
                  IF  WK-TORICD = 1994
                      MOVE WK-TAIL-REC TO  SRZ-REC
                      WRITE SRZ-REC
                      ADD   1         TO    SND-CNT5
                  END-IF
******************リック植物
                  IF  WK-TORICD = 1995
                      MOVE WK-TAIL-REC TO  SRS-REC
                      WRITE SRS-REC
                      ADD   1         TO    SND-CNT6
                  END-IF
*#2018/01/29 NAV ED↑
*************     ADD  1          TO   SND-CNT
*************     MOVE ZERO       TO   WK-TAIL-02 WK-TAIL-03
              END-IF
              MOVE     DYS-F03    TO   WK-TORICD
              MOVE     DYS-F05    TO   WK-TENCD
************* MOVE     DYS-HD00   TO   SND-REC
**************WRITE    SND-REC
**************ADD      1          TO   SND-CNT
              PERFORM  HTENMS-READ-SEC
**************ダイユーエイト／ＭＡＸ出力
              IF  WK-TORICD = 1225
                  IF  TEN-F14  =  ZERO
                      MOVE  DYS-HD00 TO   SND-REC
                      WRITE SND-REC
                      ADD   1     TO    SND-CNT1
                  ELSE
                      MOVE  DYS-HD00 TO   SMX-REC
                      WRITE SMX-REC
                      ADD   1     TO    SND-CNT2
                  END-IF
              END-IF
**************エイトファーム
              IF  WK-TORICD = 12251
                  MOVE  DYS-HD00  TO    SEF-REC
                  WRITE SEF-REC
                  ADD   1         TO    SND-CNT3
              END-IF
*2013.09.10↓
**************日敷
              IF  WK-TORICD = 12252
                  MOVE  DYS-HD00  TO    SNS-REC
                  WRITE SNS-REC
                  ADD   1         TO    SND-CNT4
              END-IF
*2013.09.10↑
*2018/01/31↓
**************リック資材
              IF  WK-TORICD = 1994
                  MOVE  DYS-HD00  TO    SRZ-REC
                  WRITE SRZ-REC
                  ADD   1         TO    SND-CNT5
              END-IF
**************リック植物
              IF  WK-TORICD = 1995
                  MOVE  DYS-HD00  TO    SRS-REC
                  WRITE SRS-REC
                  ADD   1         TO    SND-CNT6
              END-IF
*2018/01/31↑
              MOVE     DYS-F06    TO   WK-DENNO
     END-IF.
*明細部出力
*    出荷送信データ出力
     MOVE     SPACE               TO   SND-REC.
     INITIALIZE                        SND-REC.
*    ダミーデータセット
*    MOVE     "0"                 TO   DYS-A36(70:1).
*****COMPUTE  DYS-M16  =  DYS-M16 * 100.
     MOVE     DYS-ME00            TO   SND-REC.
*
*****MOVE     DYS-M02(2:13)       TO   SND-REC(3:13).
     MOVE     SPACE               TO   SND-REC(16:1).
*
     MOVE     DYS-M09A            TO   WK-NOUHIN-SU.
     MOVE     SPACE               TO   WK-NOUSU.
     MOVE     WK-NOUHIN-2         TO   WK-NOUSU-1.
     MOVE     "."                 TO   WK-NOUSU-2.
     MOVE     WK-NOUHIN-3         TO   WK-NOUSU-3.
     MOVE     WK-NOUSU            TO   SND-REC(672:8).
*
     COMPUTE  WK-GENKA  =   DYS-M10  / 100.
     MOVE     WK-GENKA            TO   SND-REC(699:10)
*
**   ADD      DYS-M10             TO   WK-TAIL-02.
**   ADD      DYS-M11             TO   WK-TAIL-03.
**** WRITE    SND-REC.
     PERFORM  HTENMS-READ-SEC.
*****ダイユーエイト／ＭＡＸ出力
     IF  WK-TORICD = 1225
         IF  TEN-F14  =  ZERO
             WRITE SND-REC
             ADD   1     TO    SND-CNT1
         ELSE
             MOVE  SND-REC TO  SMX-REC
             WRITE SMX-REC
             ADD   1     TO    SND-CNT2
         END-IF
     END-IF.
******エイトファーム
     IF  WK-TORICD = 12251
         MOVE  SND-REC   TO    SEF-REC
         WRITE SEF-REC
         ADD   1         TO    SND-CNT3
     END-IF.
*2013.09.10↓
******日敷
     IF  WK-TORICD = 12252
         MOVE  SND-REC   TO    SNS-REC
         WRITE SNS-REC
         ADD   1         TO    SND-CNT4
     END-IF.
*2013.09.10↑
*#2018/01/29 NAV ST↓
******リック資材
     IF  WK-TORICD = 1994
         MOVE  SND-REC   TO    SRZ-REC
         WRITE SRZ-REC
         ADD   1         TO    SND-CNT5
     END-IF.
******リック植物
     IF  WK-TORICD = 1995
         MOVE  SND-REC   TO    SRS-REC
         WRITE SRS-REC
         ADD   1         TO    SND-CNT6
     END-IF.
*#2018/01/29 NAV ED↑
*****ADD      1                   TO   SND-CNT.
*基本情報データにも書込み
     MOVE     SPACE               TO   DYJ-REC.
     INITIALIZE                        DYJ-REC.
     MOVE     DYS-F01             TO   DYJ-F01.
     MOVE     DYS-F02             TO   DYJ-F02.
     MOVE     DYS-F03             TO   DYJ-F03.
     MOVE     DYS-F04             TO   DYJ-F04.
     MOVE     DYS-F05             TO   DYJ-F05.
     MOVE     DYS-F06             TO   DYJ-F06.
     MOVE     DYS-F07             TO   DYJ-F07.
     MOVE     DYS-F08             TO   DYJ-F08.
     READ     DYJOHOF  INVALID
              MOVE     "INV"      TO   DYJOHOF-INV-FLG
              NOT  INVALID
              MOVE     SPACE      TO   DYJOHOF-INV-FLG
     END-READ.
*
     IF       DYJOHOF-INV-FLG = SPACE
              MOVE     SYS-DATEW           TO   DYJ-F12
              MOVE     "1"                 TO   DYJ-F11
              REWRITE  DYJ-REC
     END-IF.
*出荷送信データセット
     MOVE     SYS-DATEW           TO   DYS-F12.
     MOVE     "1"                 TO   DYS-F11.
     REWRITE  DYS-REC.
*
 MAIN010.
*    出荷情報データ読込み
     PERFORM DYSYUKF-READ-SEC.
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
     IF  SND-CNT1  >  ZERO
*2013/09/11 NAV ST 追加
     OR  SND-CNT2  >  ZERO
     OR  SND-CNT3  >  ZERO
     OR  SND-CNT4  >  ZERO
*2013/09/11 NAV ED 追加
*#2018/01/29 NAV ST
     OR  SND-CNT5  >  ZERO
     OR  SND-CNT6  >  ZERO
*#2018/01/29 NAV ED
         MOVE SPACE      TO   SND-REC
         INITIALIZE           SND-REC
         MOVE SPACE      TO   WK-TAIL-REC
         INITIALIZE           WK-TAIL-REC
         MOVE "TR"       TO   TAIL-01
*******  MOVE WK-TAIL-02 TO   TAIL-02
*******  MOVE WK-TAIL-03 TO   TAIL-03
         MOVE ALL "0"    TO   TAIL-04
*******  MOVE WK-TAIL-REC TO  SND-REC
         PERFORM  HTENMS-READ-SEC
*********ダイユーエイト／ＭＡＸ出力
         IF  WK-TORICD = 1225
             IF  TEN-F14  =  ZERO
                 MOVE WK-TAIL-REC TO  SND-REC
                 WRITE SND-REC
                 ADD   1     TO    SND-CNT1
             ELSE
                 MOVE WK-TAIL-REC TO  SMX-REC
                 WRITE SMX-REC
                 ADD   1     TO    SND-CNT2
             END-IF
         END-IF
******************エイトファーム
         IF  WK-TORICD = 12251
             MOVE WK-TAIL-REC TO   SEF-REC
             WRITE SEF-REC
             ADD   1         TO    SND-CNT3
         END-IF
*2013.09.10↓
******************日敷
         IF  WK-TORICD = 12252
             MOVE WK-TAIL-REC TO   SNS-REC
             WRITE SNS-REC
             ADD   1         TO    SND-CNT4
         END-IF
*2013.09.10↑
*#2018/01/29 NAV ST↓
******************リック資材
         IF  WK-TORICD = 1994
             MOVE WK-TAIL-REC TO   SRZ-REC
             WRITE SRZ-REC
             ADD   1         TO    SND-CNT5
         END-IF
******************リック植物
         IF  WK-TORICD = 1995
             MOVE WK-TAIL-REC TO   SRS-REC
             WRITE SRS-REC
             ADD   1         TO    SND-CNT6
         END-IF
*#2018/01/29 NAV ED↑
*********WRITE SND-REC
*********ADD  1          TO   SND-CNT
     END-IF.
*
     DISPLAY "ｼｭｯｶ   READ CNT = " READ-CNT  UPON CONS.
     DISPLAY "ｼｭｯｶ ｿｳｼﾝ ﾀﾞｲﾕｰｴｲﾄ WT = " SND-CNT1 UPON CONS.
     DISPLAY "ｼｭｯｶ ｿｳｼﾝ ｴｲﾄﾌｧｰﾑ  WT = " SND-CNT3 UPON CONS.
     DISPLAY "ｼｭｯｶ ｿｳｼﾝ ﾀﾞｲﾕｰMAX WT = " SND-CNT2 UPON CONS.
*2013.09.10↓
     DISPLAY "ｼｭｯｶ ｿｳｼﾝ ﾀﾞｲﾕｰﾆｼｷ WT = " SND-CNT4 UPON CONS.
*2013.09.10↑
*2018/01/31↓
     DISPLAY "ｼｭｯｶ ｿｳｼﾝ ﾘｯｸｼｻﾞｲ  WT = " SND-CNT5 UPON CONS.
     DISPLAY "ｼｭｯｶ ｿｳｼﾝ ﾘｯｸｼｮｸﾌﾞﾂWT = " SND-CNT6 UPON CONS.
*2018/01/31↑
     MOVE     SND-CNT1          TO      PARA-SNDCNT1.
     MOVE     SND-CNT3          TO      PARA-SNDCNT2.
     MOVE     SND-CNT2          TO      PARA-SNDCNT3.
*2013.09.10↓
     MOVE     SND-CNT4          TO      PARA-SNDCNT4.
*2013.09.10↑
*2018/01/31↓
     MOVE     SND-CNT5          TO      PARA-SNDCNT5.
     MOVE     SND-CNT6          TO      PARA-SNDCNT6.
*2018/01/31
*
     CLOSE    DYSYUKF  DYJOHOF  SNDRIC  SNDEFM  SNDMAX  HTENMS.
*2013.09.10↓
     CLOSE    SNDNIS.
*2013.09.10↑
*2018/01/31↓
     CLOSE    SNDYRZ   SNDYRS.
*2018/01/31↑
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　店舗マスタ読み込み
****************************************************************
 HTENMS-READ-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE  WK-TORICD     TO      TEN-F52.
     MOVE  WK-TENCD      TO      TEN-F011.
     READ  HTENMS
           INVALID     MOVE  SPACE   TO   TEN-REC
                       INITIALIZE         TEN-REC
     END-READ.
*
 HTENMS-READ-EXIT.
     EXIT.
