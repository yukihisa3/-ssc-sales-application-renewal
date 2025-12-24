****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ苗業務連携                    *
*    業務名　　　　　　　：　                                  *
*    モジュール名　　　　：　小売連携発注データ再抽出      *　
*    作成日／更新日　　　：　11/10/24                          *
*    作成者／更新者　　　：　ＮＡＶ畠山　                      *
*    処理概要　　　　　　：　パラメータより小売連携発注を抽出 *
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           SNA0160B.
 AUTHOR.               HATAKEYAMA.
 DATE-WRITTEN.         11/10/24.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*
*連携NO管理テーブル
     SELECT  NARKANL  ASSIGN TO    DA-01-VI-NARKANL1
             ORGANIZAITION         INDEXED
             ACCESS      MODE      RANDOM
             RECORD      KEY       NAK-F01
                         FILE      STATUS    NAK-ST.
*小売携累積ファイル
     SELECT  NARRUIF  ASSIGN TO    DA-01-VI-NARRUIL2
             ORGANIZAITION         INDEXED
             ACCESS      MODE      SEQUENTIAL
             RECORD      KEY       NAR-F01
                                   NAR-F08
                                   NAR-F09
                                   NAR-F13
                                   NAR-F18
                         FILE      STATUS    NAR-ST.
*小売連携発注データ
     SELECT  EXXXXXXA  ASSIGN TO   DA-01-S-EXXFIL
             ORGANIZAITION         SEQUENTIAL
                         FILE      STATUS    EXX-ST.
*
****************************************************************
 DATA                DIVISION.
*************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 連携NO管理テーブル　　　　　　　　　             *
****************************************************************
 FD  NARKANL
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARKANF   OF   XFDLIB
                       JOINING   NAK       AS   PREFIX.
****************************************************************
*    FILE = 小売連携累積ファイル　　　　　　　　　             *
****************************************************************
 FD  NARRUIF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      NARRUIF   OF   XFDLIB
                       JOINING   NAR       AS   PREFIX.
*
****************************************************************
*    FILE = 小売連携発注データ                                *
****************************************************************
 FD  EXXXXXXA          BLOCK     CONTAINS  1    RECORDS
                       LABEL     RECORD    IS   STANDARD.
                       COPY      EXXXXXXA  OF   XFDLIB
                       JOINING   EXX       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
 01  END-FLG                   PIC  X(01)     VALUE   SPACE.
 01  CHUSHUTU-FLG              PIC  X(01)     VALUE   SPACE.
 01  SEL-CNT                   PIC  9(06)     VALUE   ZERO.
 01  INIT-FLG                  PIC  X(01)     VALUE   SPACE.
 01  WK-RENKEI-NO              PIC  9(09)     VALUE   ZERO.
*ステータス領域
 01  STATUS-AREA.
     03  NAK-ST                PIC  X(02).
     03  NAR-ST                PIC  X(02).
     03  EXX-ST                PIC  X(02).
 01  HZK.
     03  YMD01                 PIC  9(02)     VALUE   20.
     03  YMD02                 PIC  9(06)     VALUE   ZERO.
 01  JIKAN.
     03  JIKAN01               PIC  9(06)     VALUE   ZERO.
     03  JIKAN02               PIC  9(02)     VALUE   ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  NAK-ERR           PIC N(13) VALUE
         NC"連携ＮＯ管理テーブルエラー".
     03  NAR-ERR           PIC N(13) VALUE
         NC"小売連携累積ファイルエラー".
     03  EXX-ERR           PIC N(12) VALUE
         NC"小売連携発注データエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
    03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*------------------------------------------------------------*
 01  MSG-AREA.
*----- 指定された連携NOが管理テーブルに存在しないメッセージ
*
     03  MSG-KENSU1.
         05  KENSU1A PIC N(13) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
         05  KENSU1B PIC N(13) VALUE
         NC"＊＊＊＊＊＊＊＊＊＊＊＊＊".
     03  MSG-KENSU2.
         05  KENSU2A PIC N(26) VALUE
       NC"＊指定された連携ＮＯは管理テーブルに存在しません。＊".
     03  MSG-KENSU3.
         05  KENSU3A PIC N(10) VALUE NC"＊　　　連携ＮＯ　＝".
         05  RENKEI-NO-FROM PIC X(09) VALUE SPACE.
       05  KENSU3B PIC N(11) VALUE NC"　　　　　　　　　　＊".
*----- 指定された連携NOで再抽出できない状態
     03  MSG-KENSU4.
         05  KENSU4A PIC N(26) VALUE
       NC"＊　指定された連携ＮＯは再抽出できない状態です　　＊".
     03  MSG-KENSU5.
         05  KENSU5A PIC  N(10) VALUE NC"＊　　　　状態　＝　".
         05  JOUTAI-CD   PIC  9(01)  VALUE ZERO.
         05  JOUTAI-MEI  PIC  N(08)  VALUE SPACE.
      05  KENSU5B     PIC  N(07)  VALUE NC"　　　　　　＊".
*----- 指定された連携NOが累積ファイルに存在しない
     03  MSG-KENSU6.
         05  KENSU6A PIC N(24) VALUE
       NC"指定された連携ＮＯは累積ファイルに存在しません。".
*----- 再抽出件数の表示
     03  MSG-KENSU7.
         05  KENSU7A PIC N(26) VALUE
       NC"＊　指定連携ＮＯの再抽出終了　　　　　　　　　　　＊".
     03  MSG-KENSU8.
         05  KENSU8A PIC N(10) VALUE
         NC"＊　　　　件数　　＝".
         05  CHUSHUTU-KENSUU  PIC Z(06) VALUE ZERO.
    05  KENSU8B PIC N(13) VALUE NC"　件　　　　　　　　　　＊".
*--- 状態表示
     03  JOUTAI1    PIC    N(08) VALUE NC"（データ抽出済）".
     03  JOUTAI2    PIC    N(08) VALUE NC"（苗業務取込済）".
     03  JOUTAI3    PIC    N(08) VALUE NC"（取消依頼済）".
     03  JOUTAI4    PIC    N(08) VALUE NC"（取消済）".
 LINKAGE              SECTION.
* パラメーター（B) エリア
 01  PARA-BT.
   03  PARA-BUMON                 PIC  X(04).
   03  PARA-TANTOU                PIC  X(02).
*01  PARA-BUMON                   PIC  X(04).
*01  PARA-TANTOU                  PIC  X(02).
 01  PARA-SHUBETU                 PIC  X(01).
 01  PARA-RENKEI-NO-FROM          PIC  X(09).
 01  PARA-RENKEI-NO-TO            PIC  X(09).
 01  PARA-TANMATU-IP              PIC  X(15).
*------------------------------------------------------------*
*
**************************************************************
 PROCEDURE             DIVISION  USING  PARA-BT
*                                       PARA-TANTOU
                                        PARA-SHUBETU
                                        PARA-RENKEI-NO-FROM
                                        PARA-RENKEI-NO-TO
                                        PARA-TANMATU-IP.
**************************************************************
 DECLARATIVES.
 FILEERR-SEC1              SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARKANL.
     MOVE        NAK-ST     TO       E-ST.
     MOVE        "NARKANL"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAK-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC2             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE NARRUIF.
     MOVE        NAR-ST     TO       E-ST.
     MOVE        "NARRUIF"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     NAR-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 FILEERR-SEC3             SECTION.
     USE         AFTER     EXCEPTION PROCEDURE EXXXXXXA.
     MOVE        EXX-ST      TO       E-ST.
     MOVE        "EXXXXXXA"  TO       E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     EXX-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC  THRU  MAIN-EXIT
               UNTIL  END-FLG = 1.
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT        NARRUIF.
     OPEN      I-O          NARKANL.
***  OPEN      OUTPUT       EXXXXXXA.
     ACCEPT    YMD02        FROM   DATE.
     ACCEPT    JIKAN        FROM   TIME.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*--------連携ＮＯ管理テーブルの読み込み
 MAIN010.
     MOVE  PARA-RENKEI-NO-FROM   TO NAK-F01.
     READ  NARKANL
           INVALID
     PERFORM DSP-TAISHOUNASI1-SEC THRU DSP-TAISHOUNASI1-EXIT
           MOVE   "1"        TO   END-FLG
     END-READ.
 MAIN011.
     IF  NAK-F04  IS =  "2"  OR  "1"
         PERFORM CHUSHUTU-SEC   THRU CHUSHUTU-EXIT
             UNTIL  CHUSHUTU-FLG = "1"
     ELSE
         PERFORM DSP-JOUTAIIJOU-SEC  THRU DSP-JOUTAIIJOU-EXIT
     END-IF.
     MOVE         "1"         TO   END-FLG.
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     IF       SEL-CNT  =  ZERO
              GO  TO   END010.
*
*------連携NO管理テーブルの状態を１（抽出済）に更新
     MOVE     NAK-F04               TO   NAK-F05.
     MOVE     "1"                   TO   NAK-F04.
     REWRITE  NAK-REC.
*------再抽出件数の表示
     MOVE      SEL-CNT               TO  CHUSHUTU-KENSUU.
     MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
*
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
     DISPLAY   KENSU7A                           UPON  CONS.
     DISPLAY   KENSU3A RENKEI-NO-FROM  KENSU3B   UPON  CONS.
     DISPLAY   KENSU8A CHUSHUTU-KENSUU KENSU8B  UPON  CONS.
     DISPLAY   KENSU1A KENSU1B       UPON  CONS.
 END010.
     MOVE     "END-SEC"      TO   S-NAME.
*ファイルのCLOSE処理
     CLOSE     NARKANL   NARRUIF  EXXXXXXA.
*
 END-EXIT.
     EXIT.
****************************************************************
*             小売連携発注データ　再出力処理
****************************************************************
 CHUSHUTU-SEC       SECTION.
     IF     INIT-FLG NOT = SPACE
           GO  TO  CHUSHUTU01.
*----初期処理-----------------------------------------------
     MOVE   "1"       TO    INIT-FLG.
     MOVE   NAK-F01   TO    NAR-F01.
     START  NARRUIF   KEY = NAR-F01
            INVALID
     PERFORM DSP-TAISHOUNASI2-SEC THRU DSP-TAISHOUNASI2-EXIT
            NOT INVALID
     MOVE   NAK-F01   TO    WK-RENKEI-NO
*----小売連携発注データのオープン--
     OPEN      OUTPUT       EXXXXXXA
     END-START.
*
 CHUSHUTU01.
*
     READ     NARRUIF
*****         AT  END  GO  TO  CHUSHUTU-EXIT
              AT  END
                  MOVE    "1"   TO    CHUSHUTU-FLG
                  GO  TO  CHUSHUTU-EXIT
     END-READ.
     IF       NAR-F01  NOT = WK-RENKEI-NO
              MOVE    "1"   TO    CHUSHUTU-FLG
              GO  TO   CHUSHUTU-EXIT
     ELSE
              INITIALIZE        EXX-REC
              MOVE     NAR-REC  TO  EXX-REC
*↓2012/01/31
              MOVE     NAR-F04  TO  EXX-F05
              MOVE     NAR-F05  TO  EXX-F04
*↑2012/01/31
              MOVE     HZK      TO  EXX-F98
              MOVE     JIKAN01  TO  EXX-F99
              WRITE    EXX-REC
              ADD      1  TO  SEL-CNT
     END-IF.
*
 CHUSHUTU-EXIT.
        EXIT.
**************************************************************
*        連携NO管理テーブルに対象データ無しを表示
**************************************************************
 DSP-TAISHOUNASI1-SEC SECTION.
*
      MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
      DISPLAY   KENSU1A KENSU1A       UPON  CONS.
      DISPLAY   KENSU2A               UPON  CONS.
      DISPLAY   KENSU3A RENKEI-NO-FROM  KENSU3B   UPON  CONS.
      DISPLAY   KENSU1A KENSU1A       UPON  CONS.
      MOVE    "4001"    TO  PROGRAM-STATUS.
      STOP   RUN.
 DSP-TAISHOUNASI1-EXIT.
     EXIT.
**************************************************************
*        小売連携累積ファイルに対象データ無しを表示
**************************************************************
 DSP-TAISHOUNASI2-SEC SECTION.
*
      MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
      DISPLAY   KENSU1A KENSU1A       UPON  CONS.
      DISPLAY   KENSU6A               UPON  CONS.
      DISPLAY   KENSU3A RENKEI-NO-FROM  KENSU3B   UPON  CONS.
      DISPLAY   KENSU1A KENSU1A       UPON  CONS.
      MOVE    "4001"    TO  PROGRAM-STATUS.
      STOP   RUN.
 DSP-TAISHOUNASI2-EXIT.
     EXIT.
**************************************************************
*            状態異常を表示
**************************************************************
 DSP-JOUTAIIJOU-SEC  SECTION.
*
      MOVE      PARA-RENKEI-NO-FROM   TO  RENKEI-NO-FROM.
      MOVE      NAK-F04               TO  JOUTAI-CD.
      EVALUATE  NAK-F04
*I              WHEN "1"
*I                   MOVE  JOUTAI1    TO  JOUTAI-MEI
                WHEN "3"
                     MOVE  JOUTAI2    TO  JOUTAI-MEI
                WHEN "4"
                     MOVE  JOUTAI3   TO  JOUTAI-MEI
                WHEN "5"
                     MOVE  JOUTAI4    TO  JOUTAI-MEI
      END-EVALUATE.
      DISPLAY   KENSU1A KENSU1A       UPON  CONS.
      DISPLAY   KENSU4A               UPON  CONS.
      DISPLAY   KENSU3A RENKEI-NO-FROM  KENSU3B   UPON  CONS.
      DISPLAY   KENSU5A JOUTAI-CD JOUTAI-MEI KENSU5B UPON  CONS.
      DISPLAY   KENSU1A KENSU1A       UPON  CONS.
      MOVE    "4001"    TO   PROGRAM-STATUS.
      STOP   RUN.
 DSP-JOUTAIIJOU-EXIT.
     EXIT.
